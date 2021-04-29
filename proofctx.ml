open Ast

let entail_lhs (SimpleEntail { lhs; _ }) = lhs

let entail_rhs (SimpleEntail { rhs; _ }) = rhs

type t = {
  mutable fresh_cnt : int;
  mutable precond : pi;
  mutable postcond : pi;
  mutable entails : simple_entailment list;
}

let make () = { fresh_cnt = 0; precond = True; postcond = True; entails = [] }

let clone ctx = { ctx with fresh_cnt = ctx.fresh_cnt }

let add_entail (SimpleEntail { lhs; rhs } as v) ctx =
  let entails =
    v
    :: (ctx.entails
       |> List.filter (fun x -> rhs = entail_lhs x)
       |> List.map entail_rhs
       |> List.map (fun rhs -> SimpleEntail { lhs; rhs }))
    @ (ctx.entails
      |> List.filter (fun x -> lhs = entail_rhs x)
      |> List.map entail_lhs
      |> List.map (fun lhs -> SimpleEntail { lhs; rhs }))
    @ ctx.entails
  in
  ctx.entails <- entails


let exists_entail v ctx = List.exists (( = ) v) ctx.entails

let new_term ctx =
  let no = ctx.fresh_cnt in
  ctx.fresh_cnt <- no + 1;
  Ast.Gen no


type fn_add_imply = pre:Ast.pi -> ?post:Ast.pi -> t -> unit

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
  ctx |> add_entail (Syntax.parse_simple_entailment "True && {A} |- True && {}");
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {A} |- True && {}"));
  ctx |> add_entail (Syntax.parse_simple_entailment "True && {B} |- True && {A}");
  ctx |> add_entail (Syntax.parse_simple_entailment "True && {A, B} |- True && {B}");
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {B} |- True && {}"));
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {A, B} |- True && {}"));
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {A, B} |- True && {A}"));
  ()
