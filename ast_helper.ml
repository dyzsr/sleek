open Ast

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

type term_gen = int ref

let next_term gen =
  let no = !gen in
  gen := !gen + 1;
  Ast.Gen no

let pi_of_bool = function
  | true  -> True
  | false -> False

let op_of_atomic = function
  | Eq -> ( = )
  | Lt -> ( < )
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge -> ( >= )

let total_time_of_es es gen =
  let rec aux = function
    | Sequence (es1, es2) ->
        let t1, cond1 = aux es1 in
        let t2, cond2 = aux es2 in
        (t1 +* t2, cond1 &&* cond2)
    | Union (es1, es2)    ->
        let t1, cond1 = aux es1 in
        let t2, cond2 = aux es2 in
        let t = gen |> next_term in
        (t, cond1 &&* cond2 &&* (t =* t1 ||* (t =* t2)))
    | Parallel (es1, es2) ->
        let t1, cond1 = aux es1 in
        let t2, cond2 = aux es2 in
        let t = gen |> next_term in
        (t, cond1 &&* cond2 &&* (t =* t1 &&* (t =* t2)))
    | PCases ks           ->
        let t = gen |> next_term in
        let cond =
          List.fold_left
            (fun acc (_, es) ->
              let t', cond = aux es in
              acc &&* cond &&* (t =* t'))
            True ks
        in
        (t, cond)
    | Timed (_, t)        -> (t, True)
    | _                   -> (Const 0., True)
  in
  let total, extra_conds = aux es in
  (total, extra_conds)

let rec vars_of_term acc = function
  | Var v        -> v :: acc
  | Gen n        -> ("@" ^ string_of_int n) :: acc
  | Add (t1, t2) ->
      let acc = vars_of_term acc t1 in
      let acc = vars_of_term acc t2 in
      acc
  | Sub (t1, t2) ->
      let acc = vars_of_term acc t1 in
      let acc = vars_of_term acc t2 in
      acc
  | _            -> acc

let rec visit_pi f = function
  | True               -> ()
  | False              -> ()
  | Atomic (_, t1, t2) -> f t1 t2
  | And (p1, p2)       -> visit_pi f p1; visit_pi f p2
  | Or (p1, p2)        -> visit_pi f p1; visit_pi f p2
  | Imply (p1, p2)     -> visit_pi f p1; visit_pi f p2
  | Not pi             -> visit_pi f pi

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
  let get_vars t1 t2 =
    let vars = vars_of_term [] t1 in
    let vars = vars_of_term vars t2 in
    vars
  in
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
        let vars = vars_of_term [] t in
        List.iter (fun x -> used_vars := x :: !used_vars) vars)
      terms
  in
  let pi = filter_by_relevance pi !used_vars in
  pi

let rec simplify_term : term -> term = function
  | Add (Const v1, Const v2) -> Const (v1 +. v2)
  | Add (t1, Const 0.)       -> t1
  | Add (Const 0., t2)       -> t2
  | Add (t1, t2)             -> Add (simplify_term t1, simplify_term t2)
  | Sub (Const v1, Const v2) -> Const (v1 -. v2)
  | Sub (t1, Const 0.)       -> t1
  | Sub (Const 0., t2)       -> Neg t2
  | Sub (t1, t2)             -> Sub (simplify_term t1, simplify_term t2)
  | Mul (Const v1, Const v2) -> Const (v1 *. v2)
  | Mul (_, Const 0.)        -> Const 0.
  | Mul (Const 0., _)        -> Const 0.
  | Mul (t1, Const 1.)       -> t1
  | Mul (Const 1., t2)       -> t2
  | Mul (t1, t2)             -> Mul (simplify_term t1, simplify_term t2)
  | Neg (Const v)            -> Const (-.v)
  | Neg t                    -> Neg (simplify_term t)
  | t                        -> t

let rec simplify_pi : pi -> pi = function
  (* reduction *)
  | Atomic (Eq, t1, t2) when t1 = t2 -> True
  | Atomic (op, Const v1, Const v2) ->
      let op = op_of_atomic op in
      pi_of_bool (op v1 v2)
  | Atomic (op, t1, t2) ->
      let t1' = simplify_term t1 in
      if t1' <> t1 then
        Atomic (op, t1', t2)
      else
        Atomic (op, t1, simplify_term t2)
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
  (* simplify recursively *)
  | And (pi1, pi2) ->
      let pi1' = simplify_pi pi1 in
      if pi1' <> pi1 then
        And (pi1', pi2)
      else
        And (pi1, simplify_pi pi2)
  | Or (pi1, pi2) ->
      let pi1' = simplify_pi pi1 in
      if pi1' <> pi1 then
        Or (pi1', pi2)
      else
        Or (pi1, simplify_pi pi2)
  | Not pi -> Not (simplify_pi pi)
  | Imply (pi1, pi2) ->
      let pi1' = simplify_pi pi1 in
      if pi1' <> pi1 then
        Imply (pi1', pi2)
      else
        Imply (pi1, simplify_pi pi2)
  | pi -> pi

let rec simplify_es : instants -> instants = function
  (* reduction *)
  | Union (es, Bottom) -> es
  | Union (Bottom, es) -> es
  | Union (es, es') when es = es' -> es
  | Sequence (Empty, es) -> es
  | Sequence (es, Empty) -> es
  | Sequence (Bottom, _) -> Bottom
  | Sequence (_, Bottom) -> Bottom
  | Parallel (es, Empty) -> es
  | Parallel (Empty, es) -> es
  | Parallel (_, Bottom) -> Bottom
  | Parallel (Bottom, _) -> Bottom
  | Parallel (es, es') when es = es' -> es
  | Union (Union (es1, es2), es3) -> Union (es1, Union (es2, es3))
  | Kleene (Kleene esin) -> simplify_es (Kleene esin)
  | Kleene Bottom -> Empty
  | Kleene Empty -> Empty
  | Kleene (Union (Empty, es)) -> Kleene es
  | Sequence (Sequence (es1, es2), es3) -> Sequence (es1, Sequence (es2, es3))
  (* simplify recursively *)
  | Sequence (es1, es2) ->
      let es1' = simplify_es es1 in
      if es1' <> es1 then
        Sequence (es1', es2)
      else
        Sequence (es1, simplify_es es2)
  | Union (es1, es2) ->
      let es1' = simplify_es es1 in
      if es1' <> es1 then
        Union (es1', es2)
      else
        Union (es1, simplify_es es2)
  | Parallel (es1, es2) ->
      let es1' = simplify_es es1 in
      if es1' <> es1 then
        Parallel (es1', es2)
      else
        Parallel (es1, simplify_es es2)
  | Kleene es -> Kleene (simplify_es es)
  | Timed (es, t) -> Timed (simplify_es es, t)
  | es -> es

let simplify = function
  | False, _  -> (False, Bottom)
  | _, Bottom -> (False, Bottom)
  | pi, es    ->
      let pi' = simplify_pi pi in
      if pi' <> pi then
        (pi', es)
      else
        (pi, simplify_es es)
