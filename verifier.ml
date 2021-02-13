let rec fixpoint f x = if f x = x then x else fixpoint f (f x)

let verify_entailment (Ast.Entailment { lhs; rhs }) =
  let open Ast in
  let open Inference in
  let lhs = fixpoint normalize lhs in
  let rhs = fixpoint normalize rhs in
  let rec aux lhs rhs ctx =
    if basic lhs then true
    else if disprove lhs rhs then false
    else if prove lhs rhs ctx then true
    else unfold lhs rhs ctx
  and basic lhs = lhs = Bottom
  and disprove lhs rhs = nullable lhs && not (nullable rhs)
  and prove lhs rhs ctx = Proofctx.exists (lhs, rhs) ctx
  and unfold lhs rhs ctx =
    let firsts = first lhs in
    let ctx = Proofctx.add (lhs, rhs) ctx in
    List.fold_left
      (fun res i -> res && aux (partial_deriv i lhs) (partial_deriv i rhs) ctx)
      true firsts
  in
  aux lhs rhs Proofctx.empty

let verify_specification (Ast.Spec (entailment, assertion)) =
  if verify_entailment entailment == assertion then "Correct" else "Incorrect"
