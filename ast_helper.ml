open Ast

type term_gen = int ref

let next_term gen =
  let no = !gen in
  gen := !gen + 1;
  Ast.Gen no

let ( +* ) a b = Add (a, b)
let ( -* ) a b = Sub (a, b)
let ( ** ) a b = Mul (a, b)
let ( =* ) a b = Atomic (Eq, a, b)
let ( <* ) a b = Atomic (Lt, a, b)
let ( <=* ) a b = Atomic (Le, a, b)
let ( >* ) a b = Atomic (Gt, a, b)
let ( >=* ) a b = Atomic (Ge, a, b)
let ( &&* ) a b = And (a, b)
let ( ||* ) a b = Or (a, b)
let ( =>* ) a b = Imply (a, b)
let ( !* ) a = Not a

let rec is_const = function
  | Const _      -> true
  | Var _        -> false
  | Gen _        -> false
  | Add (t1, t2) -> is_const t1 && is_const t2
  | Sub (t1, t2) -> is_const t1 && is_const t2
  | Mul (t1, t2) -> is_const t1 && is_const t2
  | Neg t        -> is_const t

let rec is_bot = function
  | Bottom              -> true
  | Empty               -> false
  | Instant _           -> false
  | Await _             -> false
  | Sequence (tr1, tr2) -> is_bot tr1 || is_bot tr2
  | Union (tr1, tr2)    -> is_bot tr1 && is_bot tr2
  | Parallel (tr1, tr2) -> is_bot tr1 || is_bot tr2
  | Kleene _            -> false
  | PCases ks           -> List.fold_left (fun acc (_, tr) -> acc || is_bot tr) false ks

let rec is_null = function
  | Bottom              -> false
  | Empty               -> true
  | Instant _           -> false
  | Await _             -> false
  | Sequence (tr1, tr2) -> is_null tr1 && is_null tr2
  | Union (tr1, tr2)    -> is_null tr1 && is_null tr2
  | Parallel (tr1, tr2) -> is_null tr1 && is_null tr2
  | Kleene _            -> false
  | PCases ks           -> List.fold_left (fun acc (_, tr) -> acc && is_null tr) true ks

let rec nullable = function
  | Bottom              -> false
  | Empty               -> true
  | Instant _           -> false
  | Await _             -> false
  | Sequence (tr1, tr2) -> nullable tr1 && nullable tr2
  | Union (tr1, tr2)    -> nullable tr1 || nullable tr2
  | Parallel (tr1, tr2) -> nullable tr1 && nullable tr2
  | Kleene _            -> true
  | PCases ks           -> List.fold_left (fun acc (_, tr) -> acc || nullable tr) false ks

let rec normalize_term : term -> term = function
  | Add (Const v1, Const v2) -> Const (v1 +. v2)
  | Add (t1, Const 0.)       -> t1
  | Add (Const 0., t2)       -> t2
  | Add (t1, Add (t2, t3))   -> Add (Add (t1, t2), t3)
  | Add (t1, t2)             -> Add (normalize_term t1, normalize_term t2)
  | Sub (Const v1, Const v2) -> Const (v1 -. v2)
  | Sub (t1, Const 0.)       -> t1
  | Sub (Const 0., t2)       -> Neg t2
  | Sub (t1, t2)             -> Sub (normalize_term t1, normalize_term t2)
  | Mul (Const v1, Const v2) -> Const (v1 *. v2)
  | Mul (_, Const 0.)        -> Const 0.
  | Mul (Const 0., _)        -> Const 0.
  | Mul (t1, Const 1.)       -> t1
  | Mul (Const 1., t2)       -> t2
  | Mul (t1, Mul (t2, t3))   -> Mul (Mul (t1, t2), t3)
  | Mul (t1, t2)             -> Mul (normalize_term t1, normalize_term t2)
  | Neg (Const v)            -> Const (-.v)
  | Neg t                    -> Neg (normalize_term t)
  | t                        -> t

let rec normalize_pi : pi -> pi = function
  (* reduction *)
  | Atomic (op, t1, t2) -> Atomic (op, normalize_term t1, normalize_term t2)
  | And (True, pi) -> pi
  | And (pi, True) -> pi
  | And (False, _) -> False
  | And (_, False) -> False
  | And (pi, pi') when pi = pi' -> pi
  | Or (True, _) -> True
  | Or (_, True) -> True
  | Or (False, pi) -> pi
  | Or (pi, False) -> pi
  | Or (pi, pi') when pi = pi' -> pi
  | Imply (False, _) -> True
  | Imply (True, pi) -> pi
  | Imply (pi, pi') when pi = pi' -> True
  | Not True -> False
  | Not False -> True
  | And (p1, And (p2, p3)) -> And (And (p1, p2), p3)
  | Or (p1, Or (p2, p3)) -> Or (Or (p1, p2), p3)
  (* normalize recursively *)
  | And (pi1, pi2) -> And (normalize_pi pi1, normalize_pi pi2)
  | Or (pi1, pi2) -> Or (normalize_pi pi1, normalize_pi pi2)
  | Imply (pi1, pi2) -> Imply (normalize_pi pi1, normalize_pi pi2)
  | Not pi -> Not (normalize_pi pi)
  | pi -> pi

let rec normalize_trace : trace -> trace = function
  (* reduction *)
  | tr' when is_bot tr' -> Bottom
  | tr' when is_null tr' -> Empty
  | Union (tr, Bottom) -> tr
  | Union (Bottom, tr) -> tr
  | Union (tr, tr') when tr = tr' -> tr
  | Sequence (Empty, tr) -> tr
  | Sequence (tr, Empty) -> tr
  | Parallel (tr, Empty) -> tr
  | Parallel (Empty, tr) -> tr
  | Parallel (tr, tr') when tr = tr' -> tr
  | Union (Union (tr1, tr2), tr3) -> Union (tr1, Union (tr2, tr3))
  | Kleene (Kleene tr) -> Kleene tr
  | Kleene Bottom -> Empty
  | Kleene Empty -> Empty
  | Sequence (Sequence (tr1, tr2), tr3) -> Sequence (tr1, Sequence (tr2, tr3))
  (* normalize recursively *)
  | Sequence (tr1, tr2) -> Sequence (normalize_trace tr1, normalize_trace tr2)
  | Union (tr1, tr2) -> Union (normalize_trace tr1, normalize_trace tr2)
  | Parallel (tr1, tr2) -> Parallel (normalize_trace tr1, normalize_trace tr2)
  | Kleene tr -> Kleene (normalize_trace tr)
  | PCases ks -> PCases (List.map (fun (p, tr) -> (normalize_term p, normalize_trace tr)) ks)
  | tr -> tr

let normalize = function
  | False, _  -> (False, Bottom)
  | _, Bottom -> (False, Bottom)
  | pi, tr    -> (normalize_pi pi, normalize_trace tr)

let vars_of_term t =
  let rec aux acc = function
    | Const _ -> acc
    | Var v -> v :: acc
    | Gen n -> ("@" ^ string_of_int n) :: acc
    | Add (t1, t2) | Sub (t1, t2) | Mul (t1, t2) -> aux (aux acc t1) t2
    | Neg t -> aux acc t
  in
  aux [] t

let terms_of_pi pi =
  let rec aux acc = function
    | True -> acc
    | False -> acc
    | Atomic (_, t1, t2) -> t1 :: t2 :: acc
    | And (p1, p2) | Or (p1, p2) | Imply (p1, p2) -> aux (aux acc p1) p2
    | Not pi -> aux acc pi
  in
  aux [] pi

let rec visit_pi f = function
  | True -> ()
  | False -> ()
  | Atomic (_, t1, t2) -> f t1 t2
  | And (p1, p2) | Or (p1, p2) | Imply (p1, p2) -> visit_pi f p1; visit_pi f p2
  | Not pi -> visit_pi f pi

let rec filter_pi f = function
  | True                     -> Some True
  | False                    -> Some False
  | Atomic (_, t1, t2) as pi -> if f t1 t2 then Some pi else None
  | And (p1, p2)             -> Utils.opt_map2 ~ab:( &&* ) (filter_pi f p1) (filter_pi f p2)
  | Or (p1, p2)              -> Utils.opt_map2 ~ab:( ||* ) (filter_pi f p1) (filter_pi f p2)
  | Imply (p1, p2)           -> Utils.opt_map2 ~ab:( =>* ) ~a:( !* ) (filter_pi f p1)
                                  (filter_pi f p2)
  | Not pi                   -> Utils.opt_map ~f:( !* ) (filter_pi f pi)

let filter_by_relevance pi used_vars =
  let get_vars t1 t2 = vars_of_term t1 @ vars_of_term t2 in
  let module Dsu = Map.Make (String) in
  let dsu = ref Dsu.empty in
  List.iter (fun x -> dsu := !dsu |> Dsu.add x (x, true)) used_vars;
  let rec find x =
    match !dsu |> Dsu.find_opt x with
    | None ->
        dsu := !dsu |> Dsu.add x (x, false);
        (x, false)
    | Some (x', rel) when x = x' -> (x', rel)
    | Some (x', _) ->
        let x', rel = find x' in
        dsu := !dsu |> Dsu.add x (x', rel);
        (x', rel)
  in
  let union x y =
    let x', rel_x = find x in
    let y', rel_y = find y in
    dsu := !dsu |> Dsu.add x' (y', rel_x || rel_y);
    dsu := !dsu |> Dsu.add y' (y', rel_x || rel_y)
  in
  let find_relevent t1 t2 =
    let vars = get_vars t1 t2 in
    let _ =
      List.fold_left
        (fun prev x ->
          Utils.opt_iter ~f:(union x) prev;
          Some x)
        None vars
    in
    ()
  in
  visit_pi find_relevent pi;
  let f t1 t2 =
    let vars = get_vars t1 t2 in
    (is_const t1 && is_const t2) || List.exists (fun x -> snd (find x)) vars
  in
  match filter_pi f pi with
  | None    -> True
  | Some pi -> pi

let trim_pi pi terms =
  let used_vars = ref [] in
  let () =
    List.iter
      (fun t ->
        let vars = vars_of_term t in
        List.iter (fun x -> used_vars := x :: !used_vars) vars)
      terms
  in
  let pi = filter_by_relevance pi !used_vars in
  pi

let amend_constraints (pi, tr) =
  let pi = ref pi in
  let rec aux = function
    | PCases ks           ->
        let total_p = List.fold_left (fun acc (p, tr) -> aux tr; p +* acc) (Const 0.) ks in
        let cond = total_p =* Const 1. in
        pi := cond &&* !pi
    | Sequence (tr1, tr2) -> aux tr1; aux tr2
    | Union (tr1, tr2)    -> aux tr1; aux tr2
    | Parallel (tr1, tr2) -> aux tr1; aux tr2
    | Kleene tr           -> aux tr
    | _                   -> ()
  in
  aux tr; (!pi, tr)
