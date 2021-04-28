open Ast

let entail_lhs (SimpleEntail { lhs; _ }) = lhs

let entail_rhs (SimpleEntail { rhs; _ }) = rhs

type t = {
  mutable fresh_cnt : int;
  mutable l_imply : pi;
  mutable r_imply : pi;
  entailments : simple_entailment list;
}

let make () = { fresh_cnt = 0; l_imply = True; r_imply = True; entailments = [] }

let add_entail (SimpleEntail { lhs; rhs } as v) ctx =
  let entailments =
    v
    :: (ctx.entailments
       |> List.filter (fun x -> rhs = entail_lhs x)
       |> List.map entail_rhs
       |> List.map (fun rhs -> SimpleEntail { lhs; rhs }))
    @ (ctx.entailments
      |> List.filter (fun x -> lhs = entail_rhs x)
      |> List.map entail_lhs
      |> List.map (fun lhs -> SimpleEntail { lhs; rhs }))
    @ ctx.entailments
  in
  { ctx with entailments }


let exists_entail v ctx = List.exists (( = ) v) ctx.entailments

let new_term ctx =
  let no = ctx.fresh_cnt in
  ctx.fresh_cnt <- no + 1;
  Ast.Gen no


type fn_add_imply = pre:Ast.pi -> ?post:Ast.pi -> t -> unit

let add_l_imply ~pre ?(post = True) ctx =
  match ctx.l_imply with
  | Imply (hd, tl) -> ctx.l_imply <- pre =>* (post =>* (hd =>* tl))
  | pi             -> ctx.l_imply <- pre =>* (post &&* pi)


let add_r_imply ~pre ?(post = True) ctx =
  match ctx.r_imply with
  | Imply (hd, tl) -> ctx.r_imply <- pre =>* (post =>* (hd =>* tl))
  | pi             -> ctx.r_imply <- pre =>* (post &&* pi)


let check_implies ctx =
  let l_imply = Utils.(fixpoint ~f:normalize_pi) ctx.l_imply in
  let r_imply = Utils.(fixpoint ~f:normalize_pi) ctx.r_imply in
  let l = not (Checker.check (Not l_imply)) in
  let r = not (Checker.check (Not r_imply)) in
  (* if l_imply <> True && l_imply <> False then
       Printf.printf "\027[2m<lhs> %s : %B\n" (show_pi l_imply) l;
     if r_imply <> True && r_imply <> False then
       Printf.printf "\027[2m<rhs> %s : %B\n" (show_pi r_imply) r; *)
  ((not l) || r, l_imply, r_imply)


(* tests *)
let () =
  let ctx = make () in
  let ctx = ctx |> add_entail (Syntax.parse_simple_entailment "True && {A} |- True && {}") in
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {A} |- True && {}"));
  let ctx =
    ctx
    |> add_entail (Syntax.parse_simple_entailment "True && {B} |- True && {A}")
    |> add_entail (Syntax.parse_simple_entailment "True && {A, B} |- True && {B}")
  in
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {B} |- True && {}"));
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {A, B} |- True && {}"));
  assert (ctx |> exists_entail (Syntax.parse_simple_entailment "True && {A, B} |- True && {A}"));
  ()
