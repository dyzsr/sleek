open Ast_helper
open Ast_print

module Set = struct
  include List

  type base_elm = {
    i : Signals.t;
    t : Ast.term option;
  }

  type elm =
    | BaseElm  of base_elm
    | WithProb of (Ast.term * base_elm) list

  let show_elm = function
    | BaseElm { i; t } ->
        Colors.magenta
        (* ^ (match p with
           | Some p -> Printf.sprintf "%s %s→%s " (show_term p) Colors.yellow Colors.magenta
           | None   -> "") *)
        ^ Signals.show i
        ^ (match t with
          | Some t -> Printf.sprintf " %s#%s %s" Colors.yellow Colors.magenta (show_term t)
          | None   -> "")
        ^ Colors.reset
    | WithProb ks      ->
        Colors.yellow ^ "{"
        ^ String.concat " | "
            (List.map
               (fun (p, { i; t }) ->
                 Colors.magenta
                 ^ Printf.sprintf "%s %s→%s " (show_term p) Colors.yellow Colors.magenta
                 ^ Signals.show i
                 ^ (match t with
                   | Some t -> Printf.sprintf " %s#%s %s" Colors.yellow Colors.magenta (show_term t)
                   | None   -> "")
                 ^ Colors.yellow)
               ks)
        ^ "}" ^ Colors.reset

  type t = elm list
  let empty = []
  let is_empty s = List.length s = 0
  let from ?t i = [ BaseElm { i; t } ]
  let union a b = a @ b |> List.sort_uniq Stdlib.compare
  let zip ctx a b =
    a
    |> List.fold_left
         (fun acc elm1 ->
           acc
           @ (b
             |> List.map (fun elm2 ->
                    match (elm1, elm2) with
                    | BaseElm { i = i1; t = t1 }, BaseElm { i = i2; t = t2 } ->
                        let i = Signals.merge i1 i2 in
                        let t = Utils.opt_map2 ~ab:(fun _ _ -> ctx |> Proofctx.next_term) t1 t2 in
                        BaseElm { i; t }
                    | _ -> failwith "not implemented")))
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
  | Kleene _            -> false
  | Timed (es, _)       -> is_bot es
  | PCases ks           -> List.fold_left (fun acc (_, es) -> acc && is_bot es) false ks

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
  | PCases ks           -> List.fold_left (fun acc (_, es) -> acc || nullable es) false ks

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
    | Kleene es -> aux es
    | Timed (es, _) ->
        aux es
        |> map (function
             | BaseElm { i; t = None } ->
                 let t = ctx |> Proofctx.next_term in
                 BaseElm { i; t = Some t }
             | BaseElm _ as elm        -> elm
             | WithProb ks             ->
                 WithProb
                   (List.map
                      (function
                        | p, { i; t = None } ->
                            let t = ctx |> Proofctx.next_term in
                            (p, { i; t = Some t })
                        | k                  -> k)
                      ks))
    | PCases ks ->
        let elmss = List.map (fun (_, es) -> aux es) ks in
        let kss = Utils.combinations elmss in
        let elms =
          kss
          |> List.map (fun ks ->
                 WithProb
                   (List.fold_left
                      (fun acc elm ->
                        match elm with
                        | BaseElm { i; t } ->
                            let p = ctx |> Proofctx.next_term in
                            (p, { i; t }) :: acc
                        | WithProb in_ks   -> in_ks @ acc)
                      [] ks))
        in
        elms
  in
  aux es

let partial_deriv ctx elm es =
  let connect_t ~t' t =
    match (t', t) with
    | Some t', Some t ->
        ctx |> Proofctx.track_term t';
        ctx |> Proofctx.add_precond (t' =* t)
    | _               -> ()
  in
  let connect_p ?ps' p =
    match (ps', p) with
    | Some ps', Some p ->
        let total_p' = List.fold_left ( +* ) (Const 0.) ps' in
        let cond = total_p' =* p in
        ctx |> Proofctx.track_term total_p';
        ctx |> Proofctx.add_precond cond
    | Some ps', None   ->
        let total_p' = List.fold_left ( +* ) (Const 0.) ps' in
        let cond = total_p' =* Const 1. in
        ctx |> Proofctx.track_term total_p';
        ctx |> Proofctx.add_precond cond
    | None, Some p     ->
        ctx |> Proofctx.track_term p;
        ctx |> Proofctx.add_precond (p =* Const 1.)
    | _                -> ()
  in
  let rec aux ?t ?p es =
    let reject =
      match t with
      | None   -> false
      | Some _ -> (
          match elm with
          | BaseElm { t = t'; _ } -> Utils.opt_none t'
          | WithProb ks'          -> List.for_all (fun (_, { t = t'; _ }) -> Utils.opt_none t') ks')
    in
    if reject then
      Bottom
    else
      match es with
      | Bottom -> Bottom
      | Empty -> Bottom
      | Instant j -> (
          match elm with
          | BaseElm { i; t = t' } ->
              if Signals.(i |- j) then (
                connect_t ~t' t; connect_p p; Empty)
              else
                Bottom
          | WithProb ks'          -> (
              match p with
              | Some p ->
                  if List.exists (fun (_, { i; _ }) -> Signals.(i |- j)) ks' then (
                    let ps' =
                      ks'
                      |> List.filter (fun (_, { i; _ }) -> Signals.(i |- j))
                      |> List.map (fun (p', { t = t'; _ }) -> connect_t ~t' t; p')
                    in
                    connect_p ~ps' (Some p); Empty)
                  else
                    Bottom
              | None   ->
                  if List.for_all (fun (_, { i; _ }) -> Signals.(i |- j)) ks' then (
                    let ps' = ks' |> List.map (fun (p', { t = t'; _ }) -> connect_t ~t' t; p') in
                    connect_p ~ps' None; Empty)
                  else
                    Bottom))
      | Await e -> (
          let j = Signals.make [ e ] in
          match t with
          | Some t -> (
              match elm with
              | BaseElm { i; t = t' } ->
                  if Signals.(i |- j) then (
                    connect_t ~t' (Some t); connect_p p; Empty)
                  else
                    let t1 = ctx |> Proofctx.next_term in
                    let t2 = ctx |> Proofctx.next_term in
                    let cond = t =* t1 +* t2 &&* (t1 >=* Const 0.) &&* (t2 >=* Const 0.) in
                    ctx |> Proofctx.add_precond cond;
                    connect_t ~t' (Some t1);
                    Timed (Await e, t2)
              | WithProb ks'          ->
                  if List.for_all (fun (_, { i; _ }) -> Signals.(i |- j)) ks' then (
                    List.iter (fun (_, { t = t'; _ }) -> connect_t ~t' (Some t)) ks';
                    let ps' = ks' |> List.map (fun (p, _) -> p) in
                    connect_p ~ps' p; Empty)
                  else if List.exists (fun (_, { i; _ }) -> Signals.(i |- j)) ks' then
                    Bottom
                  else
                    let t1 = ctx |> Proofctx.next_term in
                    let t2 = ctx |> Proofctx.next_term in
                    let cond = t =* t1 +* t2 &&* (t1 >=* Const 0.) &&* (t2 >=* Const 0.) in
                    ctx |> Proofctx.add_precond cond;
                    List.iter (fun (_, { t = t'; _ }) -> connect_t ~t' (Some t1)) ks';
                    Timed (Await e, t2))
          | _      -> (
              match elm with
              | BaseElm { i; _ } ->
                  if Signals.(i |- j) then
                    Empty
                  else
                    Await e
              | WithProb ks'     ->
                  if List.for_all (fun (_, { i; _ }) -> Signals.(i |- j)) ks' then
                    Empty
                  else if List.exists (fun (_, { i; _ }) -> Signals.(i |- j)) ks' then
                    Bottom
                  else
                    Await e))
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
                | true, true   -> False
              in
              ctx |> Proofctx.add_precond cond
          | _      -> ());
          Union (deriv1, deriv2)
      | Parallel (es1, es2) ->
          let deriv1 = aux es1 ?t ?p in
          let deriv2 = aux es2 ?t ?p in
          Parallel (deriv1, deriv2)
      | Kleene es -> aux (Sequence (es, Kleene es)) ?t
      | Timed (es, in_t) ->
          (match t with
          | Some t -> ctx |> Proofctx.add_precond (t =* in_t)
          | _      -> ());
          aux es ~t:in_t ?p
      | PCases ks ->
          let derivs =
            List.map
              (fun (in_p, es) ->
                match p with
                | None   -> (in_p, aux es ?t ~p:in_p)
                | Some p -> (in_p, aux es ?t ~p:(p ** in_p)))
              ks
          in
          let derivs =
            List.filter
              (fun (in_p, es) ->
                if is_bot es then (
                  ctx |> Proofctx.add_precond (in_p =* Const 0.);
                  false)
                else
                  true)
              derivs
          in
          PCases derivs
  in
  aux es
