open Ast

let entail_lhs (Entail { lhs; _ }) = lhs

let entail_rhs (Entail { rhs; _ }) = rhs

type t = entailment list

let empty = []

let add (Entail { lhs; rhs } as v) ctx =
  v
  :: (ctx
     |> List.filter (fun x -> rhs = entail_lhs x)
     |> List.map entail_rhs
     |> List.map (fun rhs -> Entail { lhs; rhs }))
  @ (ctx
    |> List.filter (fun x -> lhs = entail_rhs x)
    |> List.map entail_lhs
    |> List.map (fun lhs -> Entail { lhs; rhs }))
  @ ctx
;;

let exists v ctx = List.exists (( = ) v) ctx

(* tests *)
let () =
  let ctx = empty in
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
;;
