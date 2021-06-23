open Ast_helper
open Ast_print

let verify_entailment (lhs, rhs) =
  let preprocess ctx lhs rhs =
    let lhs = ctx |> Proofctx.replace_constants lhs in
    let rhs = ctx |> Proofctx.replace_constants rhs in
    let lhs = Ast_helper.amend_constraints lhs in
    let rhs = Ast_helper.amend_constraints rhs in
    (* build constraints *)
    let pre, _ = lhs in
    let post, _ = rhs in
    ctx |> Proofctx.set_precond pre;
    ctx |> Proofctx.set_postcond post;
    (lhs, rhs)
  in
  let rec aux ctx ?first lhs rhs =
    let hist = History.make_entry () in
    Utils.opt_iter ~f:(fun x -> hist |> History.set_first x) first;
    let bot_lhs (_, tr1) = is_bot tr1
    and bot_rhs (_, tr2) = is_bot tr2
    and disprove (_, tr1) (_, tr2) = nullable tr1 && not (nullable tr2)
    and reoccur ctx (_, tr1) (_, tr2) = Proofctx.exists_entail tr1 tr2 ctx
    and unfold ctx (pi1, tr1) (pi2, tr2) =
      if is_null tr1 then
        (true, true)
      else (
        ctx |> Proofctx.add_entail tr1 tr2;
        let firsts = Inference.first ctx tr1 in
        let verdict =
          firsts
          |> Inference.Set.for_all (fun first ->
                 let ctx = Proofctx.clone ctx in
                 let tr1 = Inference.partial_deriv ctx first tr1 in
                 let tr2 = Inference.partial_deriv ctx first tr2 in
                 let verdict, sub_hist = aux ctx ~first (pi1, tr1) (pi2, tr2) in
                 hist |> History.add_unfolding sub_hist;
                 verdict)
        in
        (verdict, false))
    and normalize lhs rhs =
      let lhs =
        lhs
        |> Utils.fixpoint ~f:normalize ~iter:(fun tr ->
               hist |> History.add_iteration ("NORMALIZE", (tr, rhs)))
      in
      let rhs =
        rhs
        |> Utils.fixpoint ~f:normalize ~iter:(fun tr ->
               hist |> History.add_iteration ("NORMALIZE", (lhs, tr)))
      in
      (lhs, rhs)
    in
    let check = function
      | false ->
          hist |> History.set_verdict false;
          false
      | true  ->
          let verdict, constrnt = ctx |> Proofctx.check_constraints in
          hist |> History.set_terms (ctx |> Proofctx.tracked_terms);
          hist |> History.set_constraints constrnt;
          hist |> History.set_verdict verdict;
          verdict
    in
    (* Verify *)
    let lhs, rhs = normalize lhs rhs in
    let verdict =
      if bot_lhs lhs then (
        hist |> History.add_iteration ("Bot-LHS", (lhs, rhs));
        check true)
      else if bot_rhs rhs then (
        hist |> History.add_iteration ("Bot-RHS", (lhs, rhs));
        check false)
      else if disprove lhs rhs then (
        hist |> History.add_iteration ("DISPROVE", (lhs, rhs));
        check false)
      else if reoccur ctx lhs rhs then (
        hist |> History.add_iteration ("REOCCUR", (lhs, rhs));
        check true)
      else (
        hist |> History.add_iteration ("UNFOLD", (lhs, rhs));
        let verdict, terminate = unfold ctx lhs rhs in
        if verdict && terminate then
          check true
        else
          verdict)
    in
    (verdict, hist)
  in
  let ctx = Proofctx.make () in
  let lhs, rhs = preprocess ctx lhs rhs in
  aux ctx lhs rhs

let verify_entailments (lhs, rhs) =
  let verdict, entriess =
    List.fold_left
      (fun (acc_verdict, acc_history) lhs ->
        if not acc_verdict then
          (false, acc_history)
        else
          let verdict, entries =
            List.fold_left
              (fun (acc2_verdict, acc2_history) rhs ->
                if acc2_verdict then
                  (true, acc2_history)
                else
                  let verdict, entry = verify_entailment (lhs, rhs) in
                  (verdict, entry :: acc2_history))
              (false, []) rhs
          in
          (verdict, List.rev entries :: acc_history))
      (true, []) lhs
  in
  (verdict, History.from_entries (List.rev entriess))

let verify_specification (Ast.Spec (entailments, assertion)) =
  let verdict, history = verify_entailments entailments in
  if verdict == assertion then
    (true, Colors.green ^ "Correct" ^ Colors.reset, history)
  else
    ( false,
      Printf.sprintf "%sIncorrect%s  got: %s%B%s  expect: %s%B%s" Colors.red Colors.reset
        Colors.blue verdict Colors.reset Colors.blue assertion Colors.reset,
      history )

let show_verification ~case ~no ~verdict ~verbose ~history =
  let no = string_of_int no in
  Colors.reset
  ^ Printf.sprintf "%s%-10s %s┃  %s\n" Colors.bold ("Case " ^ no) Colors.reset
      (show_specification case)
  ^ Printf.sprintf "%s\n" (History.show history ~verbose)
  ^ Printf.sprintf "%s%-10s %s┃  %s\n" Colors.bold "Verdict" Colors.reset verdict
