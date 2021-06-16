open Ast
open Ast_helper

type t = {
  term_gen : term_gen;
  mutable precond : pi;
  mutable postcond : pi;
  mutable terms : term list;
  mutable entails : (instants * instants) list;
}

let make () = { term_gen = ref 0; precond = True; postcond = True; terms = []; entails = [] }
let clone ctx = { ctx with term_gen = ctx.term_gen }

let current_term_gen ctx = ctx.term_gen
let next_term ctx = next_term ctx.term_gen

let replace_constants (pi, es) ctx =
  let pi = ref pi in
  let rec aux = function
    | Timed (es, Const v) ->
        let t = ctx |> next_term in
        let cond = t =* Const v in
        pi := cond &&* !pi;
        Timed (aux es, t)
    | Timed (es, t)       -> Timed (aux es, t)
    | PCases ks           ->
        PCases
          (List.map
             (function
               | Const v, es ->
                   let p = ctx |> next_term in
                   let cond = p =* Const v in
                   pi := cond &&* !pi;
                   (p, aux es)
               | p, es       -> (p, aux es))
             ks)
    | Sequence (es1, es2) -> Sequence (aux es1, aux es2)
    | Union (es1, es2)    -> Union (aux es1, aux es2)
    | Parallel (es1, es2) -> Parallel (aux es1, aux es2)
    | Kleene es           -> Kleene (aux es)
    | es                  -> es
  in
  let es = aux es in
  (!pi, es)

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
  let isomorphic (es1, es2) (es1', es2') =
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
    let rec aux es1 es2 =
      if es1 = es2 then
        true
      else
        match (es1, es2) with
        | Bottom, Bottom -> true
        | Empty, Empty -> true
        | Instant i, Instant j when i = j -> true
        | Await i, Await j when i = j -> true
        | Sequence (es1, es2), Sequence (es1', es2') -> aux es1 es1' && aux es2 es2'
        | Union (es1, es2), Union (es1', es2') -> aux es1 es1' && aux es2 es2'
        | Parallel (es1, es2), Parallel (es1', es2') -> aux es1 es1' && aux es2 es2'
        | Kleene es, Kleene es' -> aux es es'
        | Timed (es, t), Timed (es', t') ->
            let iso = union t t' in
            if iso then aux es es' else false
        | _ -> false
    in
    aux es1 es1' && aux es2 es2'
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
  ctx |> add_entail (Parsing.parse_instants "{A}") (Parsing.parse_instants "{}");
  assert (ctx |> exists_entail (Parsing.parse_instants "{A}") (Parsing.parse_instants "{}"));
  ctx |> add_entail (Parsing.parse_instants "{B}") (Parsing.parse_instants "{A}");
  ctx |> add_entail (Parsing.parse_instants "{A, B}") (Parsing.parse_instants "{B}");
  assert (ctx |> exists_entail (Parsing.parse_instants "{B}") (Parsing.parse_instants "{}"));
  assert (ctx |> exists_entail (Parsing.parse_instants "{A, B}") (Parsing.parse_instants "{}"));
  assert (ctx |> exists_entail (Parsing.parse_instants "{A, B}") (Parsing.parse_instants "{A}"));
  ()
