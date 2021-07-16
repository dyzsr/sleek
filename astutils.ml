open Ast

(* show ast *)

let enclose str = "(" ^ str ^ ")"
let nothing str = str

let bold str = Colors.bold ^ str ^ Colors.no_bold

let rec show_term_with_prec lprec rprec = function
  | Const n      -> string_of_float n
  | Var v        -> bold v
  | Gen n        -> bold ("v" ^ string_of_int n ^ "'")
  | Add (t1, t2) ->
      show_term_with_prec 0 50 t1 ^ "+" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing
  | Sub (t1, t2) ->
      show_term_with_prec 0 50 t1 ^ "-" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing
  | Mul (t1, t2) ->
      show_term_with_prec 0 60 t1 ^ "·" ^ show_term_with_prec 60 0 t2
      |> if lprec >= 60 || rprec > 60 then enclose else nothing
  | Neg t        -> "-" ^ show_term_with_prec 70 0 t
                    |> if lprec > 0 || rprec > 0 then enclose else nothing

let show_term p = show_term_with_prec 0 0 p

let rec show_pi_with_prec lprec rprec = function
  | True                -> "True"
  | False               -> "False"
  | Atomic (op, t1, t2) -> (
      let s1 = show_term t1 in
      let s2 = show_term t2 in
      match op with
      | Eq -> s1 ^ "=" ^ s2
      | Lt -> s1 ^ "<" ^ s2
      | Le -> s1 ^ "≤" ^ s2
      | Gt -> s1 ^ ">" ^ s2
      | Ge -> s1 ^ "≥" ^ s2)
  | And (p1, p2)        ->
      show_pi_with_prec 0 30 p1 ^ " ⋀ " ^ show_pi_with_prec 30 0 p2
      |> if lprec >= 30 || rprec > 30 then enclose else nothing
  | Or (p1, p2)         ->
      show_pi_with_prec 0 20 p1 ^ " ⋁ " ^ show_pi_with_prec 20 0 p2
      |> if lprec >= 20 || rprec > 20 then enclose else nothing
  | Imply (p1, p2)      ->
      show_pi_with_prec 0 10 p1 ^ " → " ^ show_pi_with_prec 10 0 p2
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Not p               -> bold "¬" ^ show_pi_with_prec 90 0 p

let show_pi p = Colors.default ^ show_pi_with_prec 0 0 p

let rec show_trace_with_prec lprec rprec = function
  | Bottom              -> "⏊ "
  | Empty               -> "𝝐"
  | Instant i           -> Instant.show i
  | Await e             -> Instant.show_event e ^ "?"
  | Sequence (tr1, tr2) ->
      Printf.sprintf "%s·%s" (show_trace_with_prec 0 30 tr1) (show_trace_with_prec 30 0 tr2)
      |> if lprec > 30 || rprec >= 30 then enclose else nothing
  | Union (tr1, tr2)    ->
      Printf.sprintf "%s + %s" (show_trace_with_prec 0 20 tr1) (show_trace_with_prec 20 0 tr2)
      |> if lprec > 20 || rprec >= 20 then enclose else nothing
  | Parallel (tr1, tr2) ->
      Printf.sprintf "%s ║ %s" (show_trace_with_prec 0 10 tr1) (show_trace_with_prec 10 0 tr2)
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Kleene tr           ->
      Printf.sprintf "%s*" (show_trace_with_prec 0 40 tr)
      |> if rprec >= 40 then enclose else nothing
  | PCases ks           ->
      let show_case (p, tr) =
        Printf.sprintf "%s → %s" (show_term p) (show_trace_with_prec 0 0 tr)
      in
      List.map show_case ks |> String.concat " | " |> fun x -> "[" ^ x ^ "]"

let show_trace tr = Colors.cyan ^ show_trace_with_prec 0 0 tr

let show_first = function
  | Null    -> Colors.magenta ^ "𝝐"
  | Solid i -> Colors.magenta ^ Instant.show i
  | PDist d ->
      Colors.magenta' ^ "[" ^ Colors.magenta
      ^ String.concat
          (Colors.magenta' ^ " | " ^ Colors.magenta)
          (List.map
             (function
               | p, Some i -> show_term p ^ "→" ^ Instant.show i
               | p, None   -> show_term p ^ "→𝝐")
             d)
      ^ Colors.magenta' ^ "]"

let show_path path =
  path |> List.map (fun first -> show_first first) |> String.concat (Colors.yellow ^ " · ")

let show_track =
  let show_list is = if is = [] then "𝝐" else List.map Instant.show is |> String.concat "·" in
  function
  | SolidTrack is -> Colors.magenta ^ show_list is
  | PDistTrack d  ->
      Colors.magenta' ^ "[" ^ Colors.magenta
      ^ String.concat
          (Colors.magenta' ^ " | " ^ Colors.magenta)
          (List.map (fun (p, is) -> show_term p ^ "→" ^ show_list is) d)
      ^ Colors.magenta' ^ "]"

let show_effect (pi, trace) = Printf.sprintf "%s: %s" (show_pi pi) (show_trace trace)

let show_effects l =
  let strs = List.map show_effect l in
  String.concat (Colors.bold ^ "  ⋁  " ^ Colors.no_bold) strs

let show_entail (lhs, rhs) =
  Printf.sprintf "%s  %s╞═  %s" (show_trace lhs) Colors.yellow (show_trace rhs)

let show_entailment (lhs, rhs) =
  Printf.sprintf "%s  %s╞═  %s" (show_effect lhs) Colors.yellow (show_effect rhs)

let show_entailments (lhs, rhs) =
  Printf.sprintf "%s  %s╞═  %s" (show_effects lhs) Colors.yellow (show_effects rhs)

let show_specification (Spec (entailments, assertion)) =
  Printf.sprintf "%s %s:: %B%s" (show_entailments entailments) Colors.magenta assertion Colors.reset

(* pi and terms *)

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

let vars_of_term t =
  let rec aux acc = function
    | Const _ -> acc
    | Var v -> v :: acc
    | Gen n -> ("v" ^ string_of_int n ^ "'") :: acc
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

(* trace *)
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

(* first and path *)

let merge_first first1 first2 =
  match (first1, first2) with
  | Null, first | first, Null -> first
  | Solid i1, Solid i2 -> Solid (Instant.merge i1 i2)
  | PDist d, Solid i | Solid i, PDist d ->
      PDist
        (List.map
           (function
             | p, Some i' -> (p, Some (Instant.merge i i'))
             | p, None    -> (p, Some i))
           d)
  | PDist d1, PDist d2 ->
      PDist
        (d1
        |> List.concat_map (fun (p1, i1) ->
               d2
               |> List.map (fun (p2, i2) ->
                      ( p1 ** p2,
                        match (i1, i2) with
                        | Some i1, Some i2 -> Some (Instant.merge i1 i2)
                        | Some i1, None    -> Some i1
                        | None, Some i2    -> Some i2
                        | None, None       -> None ))))

let empty_track = SolidTrack []

let cons_track first track =
  match (first, track) with
  | Null, track             -> track
  | Solid i, SolidTrack js  -> SolidTrack (i :: js)
  | PDist d, SolidTrack js  ->
      PDistTrack
        (d
        |> List.map (function
             | p, Some i -> (p, i :: js)
             | p, None   -> (p, js)))
  | Solid i, PDistTrack d   -> PDistTrack (d |> List.map (fun (p, js) -> (p, i :: js)))
  | PDist d1, PDistTrack d2 ->
      let subset p1 p2 =
        let vars1 = vars_of_term p1 in
        let vars2 = vars_of_term p2 in
        vars1 |> List.for_all (fun v -> vars2 |> List.exists (( = ) v))
      in
      let union p1 p2 =
        let vars1 = vars_of_term p1 in
        let vars2 = vars_of_term p2 in
        let vars = List.sort_uniq String.compare (vars1 @ vars2) in
        vars |> List.fold_left (fun acc v -> acc ** Var v) (Const 1.)
      in
      if
        d1
        |> List.for_all (fun (p1, _) ->
               d2 |> List.exists (fun (p2, _) -> subset p1 p2 || subset p2 p1))
      then
        PDistTrack
          (d1
          |> List.concat_map (fun (p1, i) ->
                 d2
                 |> List.filter_map (fun (p2, js) ->
                        if subset p1 p2 then
                          match i with
                          | Some i -> Some (p2, i :: js)
                          | None   -> Some (p2, js)
                        else if subset p2 p1 then
                          match i with
                          | Some i -> Some (p1, i :: js)
                          | None   -> Some (p1, js)
                        else
                          None)))
      else if
        d1
        |> List.exists (fun (p1, _) ->
               d2 |> List.exists (fun (p2, _) -> subset p1 p2 || subset p2 p1))
      then
        PDistTrack
          (d1
          |> List.concat_map (fun (p1, i) ->
                 let d =
                   d2
                   |> List.filter_map (fun (p2, js) ->
                          if subset p1 p2 then
                            match i with
                            | Some i -> Some (p2, i :: js)
                            | None   -> Some (p2, js)
                          else if subset p2 p1 then
                            match i with
                            | Some i -> Some (p1, i :: js)
                            | None   -> Some (p1, js)
                          else
                            None)
                 in
                 if d2 |> List.exists (fun (p2, _) -> not (subset p1 p2 || subset p2 p1)) then
                   match i with
                   | Some i -> (p1, [ i ]) :: d
                   | None   -> (p1, []) :: d
                 else
                   d))
      else
        PDistTrack
          (d1
          |> List.concat_map (fun (p1, i) ->
                 d2
                 |> List.map (fun (p2, js) ->
                        match i with
                        | Some i -> (union p1 p2, i :: js)
                        | None   -> (union p1 p2, js))))

let track_of_path path =
  let rec fold = function
    | []            -> empty_track
    | first :: rest -> cons_track first (fold rest)
  in
  fold path

(* misc *)

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

let normalize_effect = function
  | False, _  -> (False, Bottom)
  | _, Bottom -> (False, Bottom)
  | pi, tr    -> (normalize_pi pi, normalize_trace tr)

let normalize_track = function
  | PDistTrack d -> PDistTrack (d |> List.map (fun (p, is) -> (normalize_term p, is)))
  | tk           -> tk
