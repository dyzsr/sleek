open Ast

let entail_lhs (Entail { lhs; _ }) = lhs

let entail_rhs (Entail { rhs; _ }) = rhs

type t = {
  mutable fresh_cnt : int;
  entailments : entailment list;
}

let make () = { fresh_cnt = 0; entailments = [] }

let add (Entail { lhs; rhs } as v) ctx =
  let entailments =
    v
    :: (ctx.entailments
       |> List.filter (fun x -> rhs = entail_lhs x)
       |> List.map entail_rhs
       |> List.map (fun rhs -> Entail { lhs; rhs }))
    @ (ctx.entailments
      |> List.filter (fun x -> lhs = entail_rhs x)
      |> List.map entail_lhs
      |> List.map (fun lhs -> Entail { lhs; rhs }))
    @ ctx.entailments
  in
  { ctx with entailments }


let exists v ctx = List.exists (( = ) v) ctx.entailments

let new_term ctx =
  let no = ctx.fresh_cnt in
  ctx.fresh_cnt <- no + 1;
  Ast.Gen no


(* tests *)
let () =
  let ctx = make () in
  let ctx = ctx |> add (Syntax.parse_entailment "True && {A} |- True && {}") in
  assert (ctx |> exists (Syntax.parse_entailment "True && {A} |- True && {}"));
  let ctx =
    ctx
    |> add (Syntax.parse_entailment "True && {B} |- True && {A}")
    |> add (Syntax.parse_entailment "True && {A, B} |- True && {B}")
  in
  assert (ctx |> exists (Syntax.parse_entailment "True && {B} |- True && {}"));
  assert (ctx |> exists (Syntax.parse_entailment "True && {A, B} |- True && {}"));
  assert (ctx |> exists (Syntax.parse_entailment "True && {A, B} |- True && {A}"));
  ()
