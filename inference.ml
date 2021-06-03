open Ast_helper

module Set = struct
  include List
  type elem = {
    i : Signals.t;
    t : Ast.term option;
    p : Ast.term option;
  }
  type t = elem list

  let empty = []
  let is_empty s = List.length s = 0
  let from ?t ?p i = [ { i; t; p } ]
  let union a b = a @ b |> List.sort_uniq Stdlib.compare
  let zip ctx a b =
    a
    |> List.fold_left
         (fun acc { i = i1; t = t1; p = p1 } ->
           acc
           @ (b
             |> List.map (fun { i = i2; t = t2; p = p2 } ->
                    let i = Signals.merge i1 i2 in
                    let t = Utils.opt_map2 ~ab:(fun _ _ -> ctx |> Proofctx.next_term) t1 t2 in
                    let p = Utils.opt_map2 ~ab:( ** ) p1 p2 in
                    { i; t; p })))
         empty
    |> List.sort_uniq Stdlib.compare

  let () =
    assert (
      let ctx = Proofctx.make () in
      zip ctx (from (Signals.from "A")) (from (Signals.from "B"))
      = from Signals.(make [ present "A"; present "B" ]));
    assert (
      let ctx = Proofctx.make () in
      zip ctx (from (Signals.from "A")) (from (Signals.from "B") @ from (Signals.from "C"))
      = from Signals.(make [ present "A"; present "B" ])
        @ from Signals.(make [ present "A"; present "C" ]));
    assert (
      let ctx = Proofctx.make () in
      zip ctx
        (from Signals.empty @ from (Signals.from "A"))
        (from Signals.empty @ from (Signals.from "B"))
      = from Signals.empty
        @ from (Signals.from "A")
        @ from Signals.(make [ present "A"; present "B" ])
        @ from (Signals.from "B"));
    ()
end

open Ast
open Set

let rec is_bot = function
  | Bottom              -> true
  | Empty               -> false
  | Instant _           -> false
  | Await _             -> false
  | Sequence (es1, es2) -> is_bot es1 || is_bot es2
  | Union (es1, es2)    -> is_bot es1 && is_bot es2
  | Parallel (es1, es2) -> is_bot es1 || is_bot es2
  | PCases ks           -> List.fold_left (fun acc (_, es) -> acc && is_bot es) false ks
  | Kleene _            -> false
  | Timed (es, _)       -> is_bot es

let rec nullable = function
  | Bottom              -> false
  | Empty               -> true
  | Instant _           -> false
  | Await _             -> false
  | Sequence (es1, es2) -> nullable es1 && nullable es2
  | Union (es1, es2)    -> nullable es1 || nullable es2
  | Parallel (es1, es2) -> nullable es1 && nullable es2
  | PCases ks           -> List.fold_left (fun acc (_, es) -> acc || nullable es) false ks
  | Kleene _            -> true
  | Timed (es, _)       -> nullable es

let first ctx es =
  let rec aux = function
    | Bottom -> empty
    | Empty -> empty
    | Instant i -> from i
    | Await e -> union (from Signals.empty) (from (Signals.make [ e ]))
    | Sequence (es1, es2) when nullable es1 -> union (aux es1) (aux es2)
    | Sequence (es1, _) -> aux es1
    | Union (es1, es2) -> union (aux es1) (aux es2)
    | Parallel (es1, es2) -> zip ctx (aux es1) (aux es2)
    | PCases ks ->
        List.fold_left
          (fun acc (_, es) ->
            union acc
              (aux es
              |> map (function
                   | { i; t; p = None } ->
                       let p = ctx |> Proofctx.next_term in
                       { i; t; p = Some p }
                   | elem               -> elem)))
          empty ks
    | Kleene es -> aux es
    | Timed (es, _) ->
        aux es
        |> map (function
             | { i; t = None; p } ->
                 let t = ctx |> Proofctx.next_term in
                 { i; t = Some t; p }
             | elem               -> elem)
  in
  aux es

let partial_deriv ctx { i; t = t'; p = p' } es =
  let ps = ref [] in
  let add_prob = function
    | Some p -> ps := p :: !ps
    | _      -> ()
  in
  let rec aux ?t ?p es =
    match (t', t) with
    | None, Some _ -> Bottom
    | _            -> (
        match es with
        | Bottom -> Bottom
        | Empty -> Bottom
        | Instant j when Signals.(i |- j) ->
            (match (t', t) with
            | Some t', Some t ->
                ctx |> Proofctx.track_term t;
                ctx |> Proofctx.add_precond (t =* t')
            | _               -> ());
            add_prob p; Empty
        | Instant _ -> Bottom
        | Await e when Signals.(i |- make [ e ]) ->
            (match (t', t) with
            | Some t', Some t ->
                ctx |> Proofctx.track_term t;
                ctx |> Proofctx.add_precond (t =* t')
            | _               -> ());
            add_prob p; Empty
        | Await e ->
            let es' =
              match (t', t) with
              | Some t', Some t ->
                  let t1 = ctx |> Proofctx.next_term in
                  let t2 = ctx |> Proofctx.next_term in
                  let cond = t =* t1 +* t2 &&* (t1 >=* Const 0.) &&* (t2 >=* Const 0.) in
                  ctx |> Proofctx.add_precond cond;
                  ctx |> Proofctx.track_term t;
                  ctx |> Proofctx.add_precond (t1 =* t');
                  Timed (Await e, t2)
              | _               -> Await e
            in
            add_prob p; es'
        | Sequence (es1, es2) when nullable es1 ->
            let t1, t2, es2 =
              match t with
              | Some t ->
                  let t1 = ctx |> Proofctx.next_term in
                  let t2 = ctx |> Proofctx.next_term in
                  let cond = t =* t1 +* t2 &&* (t1 >=* Const 0.) &&* (t2 >=* Const 0.) in
                  ctx |> Proofctx.add_precond cond;
                  (Some t1, Some t2, Timed (es2, t2))
              | _      -> (None, None, es2)
            in
            let deriv1 = aux es1 ?t:t1 ?p in
            let deriv2 = aux es2 ?t:t2 ?p in
            Union (Sequence (deriv1, es2), deriv2)
        | Sequence (es1, es2) ->
            let t1, es2 =
              match t with
              | Some t ->
                  let t1 = ctx |> Proofctx.next_term in
                  let t2 = ctx |> Proofctx.next_term in
                  let cond = t =* t1 +* t2 &&* (t1 >=* Const 0.) &&* (t2 >=* Const 0.) in
                  ctx |> Proofctx.add_precond cond;
                  (Some t1, Timed (es2, t2))
              | _      -> (None, es2)
            in
            let deriv1 = aux es1 ?t:t1 ?p in
            Sequence (deriv1, es2)
        | Union (es1, es2) ->
            let t1, t2 =
              match t with
              | Some _ ->
                  let t1 = ctx |> Proofctx.next_term in
                  let t2 = ctx |> Proofctx.next_term in
                  (Some t1, Some t2)
              | _      -> (None, None)
            in
            let deriv1 = aux es1 ?t:t1 ?p in
            let deriv2 = aux es2 ?t:t2 ?p in
            (match t with
            | Some t ->
                let t1 = Utils.opt_value t1 in
                let t2 = Utils.opt_value t2 in
                let cond =
                  match (is_bot deriv1, is_bot deriv2) with
                  | false, false -> t =* t1 ||* (t =* t2)
                  | false, true  -> t =* t1
                  | true, false  -> t =* t2
                  | true, true   -> True
                in
                ctx |> Proofctx.add_precond cond
            | _      -> ());
            Union (deriv1, deriv2)
        | Parallel (es1, es2) ->
            let deriv1 = aux es1 ?t ?p in
            let deriv2 = aux es2 ?t ?p in
            Parallel (deriv1, deriv2)
        | PCases ks -> (
            let total = List.fold_left (fun acc (p, _) -> acc +* p) (Const 0.) ks in
            let cond = total =* Const 1. in
            if is_const total then
              ctx |> Proofctx.add_postcond cond
            else
              ctx |> Proofctx.add_precond cond;
            match p with
            | Some p -> PCases (List.map (fun (in_p, es) -> (in_p, aux es ?t ~p:(p ** in_p))) ks)
            | None   -> PCases (List.map (fun (in_p, es) -> (in_p, aux es ?t ~p:in_p)) ks))
        | Kleene es -> aux (Sequence (es, Kleene es)) ?t ?p
        | Timed (es, in_t) ->
            (match t with
            | Some t -> ctx |> Proofctx.add_precond (t =* in_t)
            | _      -> ());
            aux es ~t:in_t ?p)
  in
  let es' = aux es in
  let p = List.fold_left ( +* ) (Const 0.) !ps in
  (match (p', !ps) with
  | None, []    -> ()
  | None, _     ->
      let cond = p =* Const 1. in
      if is_const p then
        ctx |> Proofctx.add_postcond cond
      else
        ctx |> Proofctx.add_precond cond
  | Some p', [] -> ctx |> Proofctx.add_postcond (p' =* Const 1.)
  | Some p', _  ->
      ctx |> Proofctx.track_term p';
      ctx |> Proofctx.add_precond (p =* p'));
  es'
