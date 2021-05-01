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

let trim_irrelevant_pi (pi, es) =
  let rec get_vars acc = function
    | Var v          -> v :: acc
    | Plus (t1, t2)  ->
        let acc = get_vars acc t1 in
        let acc = get_vars acc t2 in
        acc
    | Minus (t1, t2) ->
        let acc = get_vars acc t1 in
        let acc = get_vars acc t2 in
        acc
    | _              -> acc
  in
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
    | Timed (es, t)       ->
        let rec filter = function
          | Bottom              -> false
          | Empty               -> false
          | Instant _           -> true
          | Await _             -> true
          | Sequence (es1, es2) -> filter es1 || filter es2
          | Union (es1, es2)    -> filter es1 || filter es2
          | Parallel (es1, es2) -> filter es1 || filter es2
          | Kleene es           -> filter es
          | Timed (es, _)       -> filter es
        in
        if filter es then (
          get_all_terms es;
          let vars = get_vars [] t in
          List.iter (fun x -> terms := x :: !terms) vars)
  in
  get_all_terms es;
  let module Dsu = Map.Make (String) in
  let dsu = ref Dsu.empty in
  let rel = ref Dsu.empty in
  List.iter
    (fun x ->
      dsu := !dsu |> Dsu.add x x;
      rel := !rel |> Dsu.add x true)
    !terms;
  let find x =
    match !dsu |> Dsu.find_opt x with
    | None   -> false
    | Some x -> !rel |> Dsu.find x
  in
  let union x y =
    match (!dsu |> Dsu.find_opt x, !dsu |> Dsu.find_opt y) with
    | None, None     ->
        dsu := !dsu |> Dsu.add x x;
        dsu := !dsu |> Dsu.add y x
    | Some x, None   ->
        dsu := !dsu |> Dsu.add y x;
        rel := !rel |> Dsu.add y (!rel |> Dsu.find x)
    | None, Some y   ->
        dsu := !dsu |> Dsu.add x y;
        rel := !rel |> Dsu.add x (!rel |> Dsu.find y)
    | Some x, Some y ->
        dsu := !dsu |> Dsu.add y x;
        rel := !rel |> Dsu.add y (!rel |> Dsu.find x || !rel |> Dsu.find y)
  in
  let rec find_relevant = function
    | True               -> ()
    | False              -> ()
    | Atomic (_, t1, t2) ->
        let vars = get_vars [] t1 in
        let vars = get_vars vars t2 in
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
    | And (p1, p2)       -> find_relevant p1; find_relevant p2
    | Or (p1, p2)        -> find_relevant p1; find_relevant p2
    | Imply (p1, p2)     -> find_relevant p1; find_relevant p2
    | Not pi             -> find_relevant pi
  in
  find_relevant pi;
  let rec filter_pi = function
    | True                     -> True
    | False                    -> False
    | Atomic (_, t1, t2) as pi ->
        let vars = get_vars [] t1 in
        let vars = get_vars vars t2 in
        if List.for_all (fun x -> find x) vars then pi else True
    | And (p1, p2)             -> And (filter_pi p1, filter_pi p2)
    | Or (p1, p2)              -> Or (filter_pi p1, filter_pi p2)
    | Imply (p1, p2)           -> Imply (filter_pi p1, filter_pi p2)
    | Not pi                   -> Not (filter_pi pi)
  in
  let pi = filter_pi pi in
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
