open Ast

let ( +* ) a b = Plus (a, b)

let ( -* ) a b = Minus (a, b)

let ( =* ) a b = Atomic (Eq, a, b)

let ( <* ) a b = Atomic (Lt, a, b)

let ( <=* ) a b = Atomic (Le, a, b)

let ( >* ) a b = Atomic (Gt, a, b)

let ( >=* ) a b = Atomic (Ge, a, b)

let ( &&* ) a b = And (a, b)

let ( ||* ) a b = Or (a, b)

let ( =>* ) a b = Imply (a, b)

let ( !* ) a = Not a

let rec vars_of_term acc = function
  | Var v          -> v :: acc
  | Bar v          -> v :: acc
  | Gen n          -> ("@" ^ string_of_int n) :: acc
  | Plus (t1, t2)  ->
      let acc = vars_of_term acc t1 in
      let acc = vars_of_term acc t2 in
      acc
  | Minus (t1, t2) ->
      let acc = vars_of_term acc t1 in
      let acc = vars_of_term acc t2 in
      acc
  | _              -> acc


let rec visit_pi f = function
  | True               -> ()
  | False              -> ()
  | Atomic (_, t1, t2) -> f t1 t2
  | And (p1, p2)       -> visit_pi f p1; visit_pi f p2
  | Or (p1, p2)        -> visit_pi f p1; visit_pi f p2
  | Imply (p1, p2)     -> visit_pi f p1; visit_pi f p2
  | Not pi             -> visit_pi f pi


let map2 ?(sn = fun x -> x) ?(ns = fun y -> y) ~ss = function
  | None, None     -> None
  | Some x, None   -> Some (sn x)
  | None, Some y   -> Some (ns y)
  | Some x, Some y -> Some (ss x y)


let rec filter_pi f = function
  | True                     -> Some True
  | False                    -> Some False
  | Atomic (_, t1, t2) as pi -> if f t1 t2 then Some pi else None
  | And (p1, p2)             -> map2 ~ss:( &&* ) (filter_pi f p1, filter_pi f p2)
  | Or (p1, p2)              -> map2 ~ss:( ||* ) (filter_pi f p1, filter_pi f p2)
  | Imply (p1, p2)           -> map2 ~ss:( =>* ) ~sn:( !* ) (filter_pi f p1, filter_pi f p2)
  | Not pi                   -> (
      match filter_pi f pi with
      | None   -> None
      | Some p -> Some (Not p))


let filter_by_relevance pi used_vars =
  let get_vars t1 t2 =
    let vars = vars_of_term [] t1 in
    let vars = vars_of_term vars t2 in
    vars
  in
  let module Dsu = Map.Make (String) in
  let dsu = ref Dsu.empty in
  List.iter (fun x -> dsu := !dsu |> Dsu.add x (x, true)) used_vars;
  let find x =
    match !dsu |> Dsu.find_opt x with
    | None ->
        dsu := !dsu |> Dsu.add x (x, false);
        (x, false)
    | Some (x', rel) when x = x' -> (x, rel)
    | Some (x', _) ->
        let x', rel = !dsu |> Dsu.find x' in
        dsu := !dsu |> Dsu.add x (x', rel);
        (x', rel)
  in
  let union x y =
    let x', rel_x = find x in
    let y', rel_y = find y in
    dsu := !dsu |> Dsu.add x' (x', rel_x || rel_y);
    dsu := !dsu |> Dsu.add y' (x', rel_x || rel_y)
  in
  let find_relevent t1 t2 =
    let vars = get_vars t1 t2 in
    let _ =
      List.fold_left
        (fun prev x ->
          (match prev with
          | None      -> ()
          | Some prev -> union prev x);
          Some x)
        None vars
    in
    ()
  in
  visit_pi find_relevent pi;
  let f t1 t2 =
    let vars = get_vars t1 t2 in
    List.exists (fun x -> snd (find x)) vars
  in
  match filter_pi f pi with
  | None    -> True
  | Some pi -> pi


let filter_by_occurence pi =
  let get_vars t1 t2 =
    let vars = vars_of_term [] t1 in
    let vars = vars_of_term vars t2 in
    let vars = List.sort_uniq Stdlib.compare vars in
    vars
  in
  let module Cnt = Map.Make (String) in
  let cnt = ref Cnt.empty in
  let get_cnt x =
    match !cnt |> Cnt.find_opt x with
    | None   -> 0
    | Some v -> v
  in
  let inc_cnt x =
    cnt :=
      !cnt
      |> Cnt.update x (function
           | None   -> Some 1
           | Some v -> Some (v + 1))
  in
  let dec_cnt x =
    cnt :=
      !cnt
      |> Cnt.update x (function
           | None -> failwith "invalid dec_cnt"
           | Some v when v <= 0 -> failwith "invalid dec_cnt"
           | Some v -> Some (v - 1))
  in
  let inc t1 t2 =
    let vars = get_vars t1 t2 in
    List.iter (fun x -> inc_cnt x) vars
  in
  visit_pi inc pi;
  let changing = ref true in
  while !changing do
    changing := false;
    let dec t1 t2 =
      let vars = get_vars t1 t2 in
      if List.exists (fun x -> get_cnt x == 1) vars then (
        changing := true;
        List.iter (fun x -> dec_cnt x) vars)
    in
    visit_pi dec pi
  done;
  let f t1 t2 =
    let vars = get_vars t1 t2 in
    List.for_all (fun x -> get_cnt x > 0) vars
  in
  match filter_pi f pi with
  | None    -> True
  | Some pi -> pi


let trim_constraints pi terms =
  let used_vars = ref [] in
  let () =
    List.iter
      (fun t ->
        let vars = vars_of_term [] t in
        List.iter (fun x -> used_vars := x :: !used_vars) vars)
      terms
  in
  let pi = filter_by_relevance pi !used_vars in
  let pi = filter_by_occurence pi in
  pi


let trim_simple_effects (pi, es) =
  let terms = ref [] in
  let rec get_all_terms = function
    | Bottom              -> ()
    | Empty               -> ()
    | Instant _           -> ()
    | Await _             -> ()
    | Sequence (es1, es2) -> get_all_terms es1; get_all_terms es2
    | Union (es1, es2)    -> get_all_terms es1; get_all_terms es2
    | Parallel (es1, es2) -> get_all_terms es1; get_all_terms es2
    | Kleene es           -> get_all_terms es
    | Timed (_, t)        -> terms := t :: !terms
  in
  get_all_terms es;
  let pi = trim_constraints pi !terms in
  (pi, es)


let disambiguate_simple_effects (pi, es) =
  let disambiguate_term = function
    | Var v -> Bar (String.capitalize_ascii v)
    | t     -> t
  in
  let rec disambiguate_pi pi =
    match pi with
    | True                -> True
    | False               -> False
    | Atomic (op, t1, t2) -> Atomic (op, disambiguate_term t1, disambiguate_term t2)
    | And (p1, p2)        -> And (disambiguate_pi p1, disambiguate_pi p2)
    | Or (p1, p2)         -> Or (disambiguate_pi p1, disambiguate_pi p2)
    | Imply (p1, p2)      -> Imply (disambiguate_pi p1, disambiguate_pi p2)
    | Not pi              -> Not (disambiguate_pi pi)
  in
  let rec disambiguate_es es =
    match es with
    | Bottom | Empty | Instant _ | Await _ -> es
    | Sequence (es1, es2) -> Sequence (disambiguate_es es1, disambiguate_es es2)
    | Union (es1, es2) -> Union (disambiguate_es es1, disambiguate_es es2)
    | Parallel (es1, es2) -> Parallel (disambiguate_es es1, disambiguate_es es2)
    | Kleene es -> Kleene (disambiguate_es es)
    | Timed (es, t) -> Timed (disambiguate_es es, disambiguate_term t)
  in
  (disambiguate_pi pi, disambiguate_es es)


let rec normalize_pi : pi -> pi = function
  (* reduction *)
  | Atomic (Eq, t1, t2) when t1 = t2 -> True
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
  | And (pi1, pi2) ->
      let pi1' = normalize_pi pi1 in
      if pi1' <> pi1 then
        And (pi1', pi2)
      else
        And (pi1, normalize_pi pi2)
  | Or (pi1, pi2) ->
      let pi1' = normalize_pi pi1 in
      if pi1' <> pi1 then
        Or (pi1', pi2)
      else
        Or (pi1, normalize_pi pi2)
  | Not pi -> Not (normalize_pi pi)
  | Imply (pi1, pi2) ->
      let pi1' = normalize_pi pi1 in
      if pi1' <> pi1 then
        Imply (pi1', pi2)
      else
        Imply (pi1, normalize_pi pi2)
  | pi -> pi


let rec normalize_es : instants -> instants = function
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
  | Kleene Bottom -> Empty
  | Kleene Empty -> Empty
  | Kleene (Union (Empty, es)) -> Kleene es
  | Sequence (Sequence (es1, es2), es3) -> Sequence (es1, Sequence (es2, es3))
  (* normalize recursively *)
  | Sequence (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Sequence (es1', es2)
      else
        Sequence (es1, normalize_es es2)
  | Union (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Union (es1', es2)
      else
        Union (es1, normalize_es es2)
  | Parallel (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Parallel (es1', es2)
      else
        Parallel (es1, normalize_es es2)
  | Kleene es -> Kleene (normalize_es es)
  | Timed (es, t) -> Timed (normalize_es es, t)
  | es -> es


let normalize = function
  | False, _  -> (False, Bottom)
  | _, Bottom -> (False, Bottom)
  | pi, es    ->
      let pi' = normalize_pi pi in
      if pi' <> pi then
        (pi', es)
      else
        (pi, normalize_es es)
