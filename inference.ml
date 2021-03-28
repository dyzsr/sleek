module Set = struct
  include List

  type t = (Signals.t * Ast.pi * Ast.term) list

  let empty = []

  let from s pi t = [ (s, pi, t) ]

  let union a b = a @ b

  let zip ctx a b =
    a
    |> List.fold_left
         (fun acc (es1, pi1, t1) ->
           acc
           @ (b
             |> List.map (fun (es2, pi2, t2) ->
                    let es' = Signals.merge es1 es2 in
                    let pi = Ast.(pi1 &&* pi2) in
                    let t' = ctx |> Proofctx.new_term in
                    let pi' = Ast.(pi &&* (t' <=* t1 ||* (t' <=* t2))) in
                    (es', pi', t'))))
         empty
    |> List.sort_uniq compare


  let () =
    let open Ast in
    let open Signals in
    assert (
      let ctx = Proofctx.make () in
      zip ctx [ (from "A", True, Nil) ] [ (from "B", True, Nil) ]
      = [
          ( make [ present "A"; present "B" ],
            True &&* True &&* (Gen 0 <=* Nil ||* (Gen 0 <=* Nil)),
            Gen 0 );
        ]);
    assert (
      let ctx = Proofctx.make () in
      zip ctx [ (from "A", True, Nil) ] [ (from "B", True, Nil); (from "C", True, Nil) ]
      = [
          ( make [ present "A"; present "B" ],
            True &&* True &&* (Gen 0 <=* Nil ||* (Gen 0 <=* Nil)),
            Gen 0 );
          ( make [ present "A"; present "C" ],
            True &&* True &&* (Gen 1 <=* Nil ||* (Gen 1 <=* Nil)),
            Gen 1 );
        ]);
    assert (
      let ctx = Proofctx.make () in
      zip ctx
        [ (empty, True, Nil); (from "A", True, Nil) ]
        [ (empty, True, Nil); (from "B", True, Nil) ]
      = [
          (empty, True &&* True &&* (Gen 0 <=* Nil ||* (Gen 0 <=* Nil)), Gen 0);
          (from "A", True &&* True &&* (Gen 2 <=* Nil ||* (Gen 2 <=* Nil)), Gen 2);
          ( make [ present "A"; present "B" ],
            True &&* True &&* (Gen 3 <=* Nil ||* (Gen 3 <=* Nil)),
            Gen 3 );
          (from "B", True &&* True &&* (Gen 1 <=* Nil ||* (Gen 1 <=* Nil)), Gen 1);
        ]);
    ()
end

let rec normalize_es : Ast.instants -> Ast.instants = function
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
  | Sequence (es, Union (es1, es2)) -> Union (Sequence (es, es1), Sequence (es, es2))
  | Sequence (Union (es1, es2), es) -> Union (Sequence (es1, es), Sequence (es2, es))
  | Parallel (es, Union (es1, es2)) -> Union (Parallel (es, es1), Parallel (es, es2))
  | Parallel (Union (es1, es2), es) -> Union (Parallel (es1, es), Parallel (es2, es))
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
  | es -> es


let rec normalize_pi : Ast.pi -> Ast.pi = function
  (* reduction *)
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
  | Imply (pi, pi') when pi = pi' -> True
  | Not True -> False
  | Not False -> True
  | Atomic (Lt, _, Nil) -> True
  | Atomic (Lte, _, Nil) -> True
  | Atomic (Gt, Nil, _) -> True
  | Atomic (Gte, Nil, _) -> True
  | And (And (p1, p2), p3) -> And (p1, And (p2, p3))
  | Or (Or (p1, p2), p3) -> Or (p1, Or (p2, p3))
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


let check_pi _ = true

let normalize : Ast.effects -> Ast.effects = function
  | False, _ -> (False, Bottom)
  | pi, es   ->
      let pi' = normalize_pi pi in
      if pi' <> pi then
        (normalize_pi pi, es)
      else if check_pi pi then
        (pi, normalize_es es)
      else
        (False, Bottom)


let rec nullable : Ast.instants -> bool = function
  | Bottom              -> false
  | Empty               -> true
  | Instant _           -> false
  | Await _             -> false
  | Sequence (es1, es2) -> nullable es1 && nullable es2
  | Union (es1, es2)    -> nullable es1 || nullable es2
  | Parallel (es1, es2) -> nullable es1 && nullable es2
  | Kleene _            -> true
  | Timed (es, _)       -> nullable es


let first ctx pi es =
  let rec aux : Ast.instants -> Set.t = function
    | Bottom -> Set.empty
    | Empty -> Set.empty
    | Instant i -> Set.from i pi Ast.Nil
    | Await e -> Set.(union (from Signals.empty pi Ast.Nil) (from (Signals.make [ e ]) pi Ast.Nil))
    | Sequence (es1, es2) when nullable es1 -> Set.union (aux es1) (aux es2)
    | Sequence (es1, _) -> aux es1
    | Union (es1, es2) -> Set.union (aux es1) (aux es2)
    | Parallel (es1, es2) -> Set.zip ctx (aux es1) (aux es2)
    | Kleene es -> aux es
    | Timed (Instant i, t) ->
        let t' = ctx |> Proofctx.new_term in
        Set.from i Ast.(pi &&* (t' >* Const 0) &&* (t' =* t)) t'
    | Timed (es, t) ->
        let attach_term (i, pi, _) =
          let t' = ctx |> Proofctx.new_term in
          let pi' = Ast.(pi &&* (t' >* Const 0) &&* (t' <=* t)) in
          (i, pi', t')
        in
        aux es |> Set.map attach_term
  in
  aux es


let partial_deriv ctx (i, pi', t') (pi, es) =
  let rec aux : Ast.instants -> Ast.effects = function
    | Bottom -> (False, Bottom)
    | Empty -> (False, Bottom)
    | Instant j when Signals.(i |- j) -> (pi, Empty)
    | Instant _ -> (False, Bottom)
    | Await e when Signals.(i |- make [ e ]) -> (pi, Empty)
    | Await e -> (pi, Await e)
    | Sequence (es1, es2) when nullable es1 ->
        let pi1, deriv1 = aux es1 in
        let pi2, deriv2 = aux es2 in
        Ast.(pi1 ||* pi2, Union (Sequence (deriv1, es2), deriv2))
    | Sequence (es1, es2) ->
        let pi, deriv1 = aux es1 in
        (pi, Sequence (deriv1, es2))
    | Union (es1, es2) ->
        let pi1, deriv1 = aux es1 in
        let pi2, deriv2 = aux es2 in
        Ast.(pi1 ||* pi2, Union (deriv1, deriv2))
    | Parallel (es1, es2) ->
        let pi1, deriv1 = aux es1 in
        let pi2, deriv2 = aux es2 in
        Ast.(pi1 &&* pi2, Parallel (deriv1, deriv2))
    | Kleene es ->
        let pi, deriv = aux es in
        (pi, Sequence (deriv, Kleene es))
    | Timed (Instant j, t) when Signals.(i |- j) ->
        let new_pi = Ast.(pi &&* (pi' &&* (t' =* t) =>* pi)) in
        (new_pi, Empty)
    | Timed (Instant _, _) -> (False, Bottom)
    | Timed (es, t) ->
        let pi, deriv = aux es in
        let t'' = ctx |> Proofctx.new_term in
        let pi' = Ast.(pi &&* (t'' <=* t &&* (t'' =* t') =>* True)) in
        (pi', deriv)
  in
  aux es
