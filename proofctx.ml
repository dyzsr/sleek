open Ast
open Ast_utils

type t = {
  fresh_cnt : int ref;
  mutable precond : pi;
  mutable postcond : pi;
  mutable terms : term list;
  mutable entails : (instants * instants) list;
}

let make () = { fresh_cnt = ref 0; precond = True; postcond = True; terms = []; entails = [] }

let clone ctx = { ctx with fresh_cnt = ctx.fresh_cnt }

let add_entail lhs rhs ctx =
  let entails =
    (lhs, rhs)
    :: (ctx.entails
       |> List.filter (fun (lhs', _) -> rhs = lhs')
       |> List.map (fun (_, rhs') -> (lhs, rhs')))
    @ (ctx.entails
      |> List.filter (fun (_, rhs') -> lhs = rhs')
      |> List.map (fun (lhs', _) -> (lhs', rhs)))
    @ ctx.entails
  in
  ctx.entails <- entails


let exists_entail lhs rhs ctx = List.exists (( = ) (lhs, rhs)) ctx.entails

let new_term ctx =
  let no = !(ctx.fresh_cnt) in
  ctx.fresh_cnt := no + 1;
  Ast.Gen no


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
  let constrnt = Ast_utils.trim_constraints constrnt (ctx |> tracked_terms) in
  let constrnt = Utils.fixpoint ~f:normalize_pi constrnt in
  (not (Checker.check (Not constrnt)), constrnt)


(* tests *)
let () =
  let ctx = make () in
  ctx |> add_entail (Syntax.parse_instants "{A}") (Syntax.parse_instants "{}");
  assert (ctx |> exists_entail (Syntax.parse_instants "{A}") (Syntax.parse_instants "{}"));
  ctx |> add_entail (Syntax.parse_instants "{B}") (Syntax.parse_instants "{A}");
  ctx |> add_entail (Syntax.parse_instants "{A, B}") (Syntax.parse_instants "{B}");
  assert (ctx |> exists_entail (Syntax.parse_instants "{B}") (Syntax.parse_instants "{}"));
  assert (ctx |> exists_entail (Syntax.parse_instants "{A, B}") (Syntax.parse_instants "{}"));
  assert (ctx |> exists_entail (Syntax.parse_instants "{A, B}") (Syntax.parse_instants "{A}"));
  ()
