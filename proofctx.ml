open Ast
open Ast_helper

type t = {
  term_gen : term_gen;
  mutable precond : pi;
  mutable postcond : pi;
  mutable terms : term list;
  mutable entails : (trace * trace) list;
}

let make () = { term_gen = ref 0; precond = True; postcond = True; terms = []; entails = [] }
let clone ctx = { ctx with term_gen = ctx.term_gen }

let current_term_gen ctx = ctx.term_gen
let next_term ctx = next_term ctx.term_gen

let replace_constants (pi, tr) ctx =
  let pi = ref pi in
  let rec aux = function
    | Timed (tr, Const v) ->
        let t = ctx |> next_term in
        let cond = t =* Const v in
        pi := cond &&* !pi;
        Timed (aux tr, t)
    | Timed (tr, t)       -> Timed (aux tr, t)
    | PCases ks           ->
        PCases
          (List.map
             (function
               | Const v, tr ->
                   let p = ctx |> next_term in
                   let cond = p =* Const v in
                   pi := cond &&* !pi;
                   (p, aux tr)
               | p, tr       -> (p, aux tr))
             ks)
    | Sequence (tr1, tr2) -> Sequence (aux tr1, aux tr2)
    | Union (tr1, tr2)    -> Union (aux tr1, aux tr2)
    | Parallel (tr1, tr2) -> Parallel (aux tr1, aux tr2)
    | Kleene tr           -> Kleene (aux tr)
    | tr                  -> tr
  in
  let tr = aux tr in
  (!pi, tr)

let add_entail lhs rhs ctx =
  let entails =
    (lhs, rhs)
    ::
    (ctx.entails
    |> List.filter (fun (lhs', _) -> rhs = lhs')
    |> List.map (fun (_, rhs') -> (lhs, rhs')))
    @ (ctx.entails
      |> List.filter (fun (_, rhs') -> lhs = rhs')
      |> List.map (fun (lhs', _) -> (lhs', rhs)))
    @ ctx.entails
  in
  ctx.entails <- entails

let exists_entail lhs rhs ctx =
  let isomorphic (tr1, tr2) (tr1', tr2') =
    let module Terms = Map.Make (struct
      type t = term

      let compare = Stdlib.compare
    end) in
    let forw = ref Terms.empty in
    let back = ref Terms.empty in
    let union t1 t2 =
      match (!forw |> Terms.find_opt t1, !back |> Terms.find_opt t2) with
      | None, None         ->
          forw := !forw |> Terms.add t1 t2;
          back := !back |> Terms.add t2 t1;
          true
      | Some t2', Some t1' -> t1 = t1' && t2 = t2'
      | _                  -> false
    in
    let rec aux tr1 tr2 =
      if tr1 = tr2 then
        true
      else
        match (tr1, tr2) with
        | Bottom, Bottom -> true
        | Empty, Empty -> true
        | Instant i, Instant j when i = j -> true
        | Await i, Await j when i = j -> true
        | Sequence (tr1, tr2), Sequence (tr1', tr2') -> aux tr1 tr1' && aux tr2 tr2'
        | Union (tr1, tr2), Union (tr1', tr2') -> aux tr1 tr1' && aux tr2 tr2'
        | Parallel (tr1, tr2), Parallel (tr1', tr2') -> aux tr1 tr1' && aux tr2 tr2'
        | Kleene tr, Kleene tr' -> aux tr tr'
        | Timed (tr, t), Timed (tr', t') ->
            let iso = union t t' in
            if iso then aux tr tr' else false
        | _ -> false
    in
    aux tr1 tr1' && aux tr2 tr2'
  in
  List.exists (isomorphic (lhs, rhs)) ctx.entails

let add_precond cond ctx = ctx.precond <- cond =>* ctx.precond
let add_postcond cond ctx = ctx.postcond <- cond &&* ctx.postcond

let track_term term ctx = ctx.terms <- term :: ctx.terms
let tracked_terms ctx =
  ctx.terms <- List.sort_uniq Stdlib.compare ctx.terms;
  ctx.terms

let check_constraints ctx =
  let constrnt =
    let rec aux = function
      | Imply (hd, tl) -> Imply (hd, aux tl)
      | pi             -> Imply (pi, ctx.postcond)
    in
    aux ctx.precond
  in
  let constrnt = Ast_helper.trim_pi constrnt (ctx |> tracked_terms) in
  let constrnt = Utils.fixpoint ~f:simplify_pi constrnt in
  (not (Checker.check (Not constrnt)), constrnt)

(* tests *)
let () =
  let ctx = make () in
  ctx |> add_entail (Parsing.trace "{A}") (Parsing.trace "{}");
  assert (ctx |> exists_entail (Parsing.trace "{A}") (Parsing.trace "{}"));
  ctx |> add_entail (Parsing.trace "{B}") (Parsing.trace "{A}");
  ctx |> add_entail (Parsing.trace "{A, B}") (Parsing.trace "{B}");
  assert (ctx |> exists_entail (Parsing.trace "{B}") (Parsing.trace "{}"));
  assert (ctx |> exists_entail (Parsing.trace "{A, B}") (Parsing.trace "{}"));
  assert (ctx |> exists_entail (Parsing.trace "{A, B}") (Parsing.trace "{A}"));
  ()
