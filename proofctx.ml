open Ast
open Ast_utils

type t = {
  mutable fresh_cnt : int;
  mutable precond : pi;
  mutable postcond : pi;
  mutable entails : (Ast.instants * Ast.instants) list;
}

let make () = { fresh_cnt = 0; precond = True; postcond = True; entails = [] }

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
  let no = ctx.fresh_cnt in
  ctx.fresh_cnt <- no + 1;
  Ast.Gen no


let add_precond cond ctx = ctx.precond <- cond =>* ctx.precond

let add_postcond cond ctx = ctx.postcond <- cond &&* ctx.postcond

let check_imply ctx =
  let imply =
    let rec aux = function
      | Imply (hd, tl) -> Imply (hd, aux tl)
      | pi             -> Imply (pi, ctx.postcond)
    in
    aux ctx.precond
  in
  let imply = Utils.fixpoint ~f:normalize_pi imply in
  (not (Checker.check (Not imply)), imply)


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
