open Ast

module Set = struct
  include List

  type elem = Signals.t * Ast.pi * Ast.term option

  type t = elem list

  let empty = []

  let is_empty = function
    | [] -> true
    | _  -> false


  let from s pi t = [ (s, pi, t) ]

  let union a b = a @ b |> List.sort_uniq compare

  let zip ctx a b =
    a
    |> List.fold_left
         (fun acc (es1, pi1, t1) ->
           acc
           @ (b
             |> List.map (fun (es2, pi2, t2) ->
                    let es' = Signals.merge es1 es2 in
                    let pi = pi1 &&* pi2 in
                    match (t1, t2) with
                    | None, None -> (es', pi, None)
                    | _          ->
                        let t' = ctx |> Proofctx.new_term in
                        let pi' =
                          match (t1, t2) with
                          | None, None       -> pi
                          | None, Some t2    -> pi &&* (t' <=* t2)
                          | Some t1, None    -> pi &&* (t' <=* t1)
                          | Some t1, Some t2 -> pi &&* (t' <=* t1 ||* (t' <=* t2))
                        in
                        (es', pi', Some t'))))
         empty
    |> List.sort_uniq compare


  let () =
    let open Ast in
    let open Signals in
    assert (
      let ctx = Proofctx.make () in
      zip ctx [ (from "A", True, None) ] [ (from "B", True, None) ]
      = [ (make [ present "A"; present "B" ], True &&* True, None) ]);
    assert (
      let ctx = Proofctx.make () in
      zip ctx [ (from "A", True, None) ] [ (from "B", True, None); (from "C", True, None) ]
      = [
          (make [ present "A"; present "B" ], True &&* True, None);
          (make [ present "A"; present "C" ], True &&* True, None);
        ]);
    assert (
      let ctx = Proofctx.make () in
      zip ctx
        [ (empty, True, None); (from "A", True, None) ]
        [ (empty, True, None); (from "B", True, None) ]
      = [
          (empty, True &&* True, None);
          (from "A", True &&* True, None);
          (make [ present "A"; present "B" ], True &&* True, None);
          (from "B", True &&* True, None);
        ]);
    ()
end

let rec nullable = function
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
  let rec aux = function
    | Bottom -> Set.empty
    | Empty -> Set.empty
    | Instant i -> Set.from i pi None
    | Await e -> Set.(union (from Signals.empty pi None) (from (Signals.make [ e ]) pi None))
    | Sequence (es1, es2) when nullable es1 -> Set.union (aux es1) (aux es2)
    | Sequence (es1, _) -> aux es1
    | Union (es1, es2) -> Set.union (aux es1) (aux es2)
    | Parallel (es1, es2) -> Set.zip ctx (aux es1) (aux es2)
    | Kleene es -> aux es
    | Timed (Bottom, _) -> Set.empty
    | Timed (Empty, _) -> Set.empty
    | Timed (Instant i, t) ->
        let t' = ctx |> Proofctx.new_term in
        Set.from i (pi &&* (t' >=* Const 0) &&* (t' =* t)) (Some t')
    | Timed (Union (es1, es2), t) -> aux (Union (Timed (es1, t), Timed (es2, t)))
    | Timed (Parallel (es1, es2), t) -> aux (Parallel (Timed (es1, t), Timed (es2, t)))
    | Timed ((Timed (_, t1) as es), t2) ->
        aux es
        |> Set.map (fun (i, pi, inner_t) ->
               match inner_t with
               | None         -> failwith "impossible"
               | Some inner_t ->
                   let pi' = pi &&* (t1 =* t2) in
                   (i, pi', Some inner_t))
    | Timed (es, t) ->
        aux es
        |> Set.map (fun (i, pi, inner_t) ->
               match inner_t with
               | None         ->
                   let t' = ctx |> Proofctx.new_term in
                   let pi' = pi &&* (t' >=* Const 0) &&* (t' <=* t) in
                   (i, pi', Some t')
               | Some inner_t ->
                   let pi' = pi &&* (inner_t <=* t) in
                   (i, pi', Some inner_t))
  in
  aux es


let _check_imply p1 p2 =
  let imply = p1 =>* p2 in
  let sat = not (Checker.check (Not imply)) in
  Printf.printf "%s : %B\n" (show_pi imply) sat;
  sat


let partial_deriv ctx (add_imply : Proofctx.fn_add_imply) (i, pi', t') (pi, es) =
  let rec aux (pi, es) =
    match es with
    | Bottom -> (False, Bottom)
    | Empty -> (False, Bottom)
    | Instant j when Signals.(i |- j) -> (pi, Empty)
    | Instant _ -> (False, Bottom)
    | Await e when Signals.(i |- make [ e ]) -> (pi, Empty)
    | Await e -> (pi, Await e)
    | Sequence (es1, es2) when nullable es1 ->
        let pi1, deriv1 = aux (pi, es1) in
        let pi2, deriv2 = aux (pi, es2) in
        (pi1 ||* pi2, Union (Sequence (deriv1, es2), deriv2))
    | Sequence (es1, es2) ->
        let pi, deriv1 = aux (pi, es1) in
        (pi, Sequence (deriv1, es2))
    | Union (es1, es2) ->
        let pi1, deriv1 = aux (pi, es1) in
        let pi2, deriv2 = aux (pi, es2) in
        (pi1 ||* pi2, Union (deriv1, deriv2))
    | Parallel (es1, es2) ->
        let pi1, deriv1 = aux (pi, es1) in
        let pi2, deriv2 = aux (pi, es2) in
        (pi1 &&* pi2, Parallel (deriv1, deriv2))
    | Kleene es ->
        let pi, deriv = aux (pi, es) in
        (pi, Sequence (deriv, Kleene es))
    | Timed (es, t) ->
    match t' with
    | None    -> (False, Bottom)
    | Some t' ->
    match es with
    | Bottom -> (False, Bottom)
    | Empty -> (False, Bottom)
    | Instant j when Signals.(i |- j) ->
        let pre = pi' &&* (t' =* t) in
        let post = pi in
        ctx |> add_imply ~pre ~post;
        (True, Empty)
    | Instant _ -> (False, Bottom)
    | Sequence (es1, es2) ->
        let t1 = ctx |> Proofctx.new_term in
        let t2 = ctx |> Proofctx.new_term in
        let pi, es = aux (pi, Sequence (Timed (es1, t1), Timed (es2, t2))) in
        let pi = pi &&* (t1 >=* Const 0) &&* (t2 >=* Const 0) &&* (t1 +* t2 =* t) in
        (pi, es)
    | Union (es1, es2) -> aux (pi, Union (Timed (es1, t), Timed (es2, t)))
    | Parallel (es1, es2) -> aux (pi, Parallel (Timed (es1, t), Timed (es2, t)))
    (* | Kleene es ->
        let t1 = ctx |> Proofctx.new_term in
        let t2 = ctx |> Proofctx.new_term in
        let pi = pi &&* (t1 >=* Const 0) &&* (t2 >=* Const 0) &&* (t1 +* t2 =* t) in
        let es =
        aux (pi, es) *)
    | Timed (_, inner_t) ->
        let pi, es = aux (pi, es) in
        let pre = pi &&* (t =* inner_t) in
        (pre, es)
        (* ctx |> add_imply ~pre;
           (True, es) *)
    | _ -> failwith "not implemented"
  in
  aux (pi, es)
