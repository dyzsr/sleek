open Ast_helper
open Ast_print

let verify_entailment (lhs, rhs) =
  let preprocess ctx lhs rhs =
    let lhs = ctx |> Proofctx.replace_constants lhs in
    let rhs = ctx |> Proofctx.replace_constants rhs in
    let lhs = Ast_helper.amend_constraints lhs in
    let rhs = Ast_helper.amend_constraints rhs in
    (* build constraints *)
    let pre, tr1 = lhs in
    let post, tr2 = rhs in
    let pre = Utils.fixpoint ~f:normalize_pi pre in
    let post = Utils.fixpoint ~f:normalize_pi post in
    ctx |> Proofctx.set_precond pre;
    ctx |> Proofctx.set_postcond post;
    let lhs = (pre, tr1) in
    let rhs = (post, tr2) in
    (lhs, rhs)
  in
  let rec aux ctx ?first lhs rhs =
    let hist = History.make_entry () in
    hist |> History.set_constraints (ctx |> Proofctx.constraints);
    Utils.opt_iter ~f:(fun (l, r) -> hist |> History.set_first l r) first;
    let check = function
      | false -> false
      | true  ->
          let verdict, comment = ctx |> Proofctx.check_constraints in
          hist |> History.set_terms (ctx |> Proofctx.tracked_terms);
          hist |> History.set_comment comment;
          verdict
    in
    let bot_lhs lhs = is_bot lhs
    and bot_rhs rhs = is_bot rhs
    and disprove lhs rhs = nullable lhs && not (nullable rhs)
    and reoccur ctx lhs rhs = ctx |> Proofctx.exists_entail lhs rhs
    and unfold ctx lhs rhs =
      if is_null lhs then
        (true, true)
      else if is_null rhs then
        (false, true)
      else (
        ctx |> Proofctx.add_entail lhs rhs;
        let lfirsts = Rewriting.first lhs in
        let rfirsts = Rewriting.first rhs in
        let verdict =
          lfirsts
          |> Rewriting.Firsts.for_all (fun lfirst ->
                 let deriv1 = Rewriting.derivative lfirst lhs in
                 let deriv2 = Rewriting.derivative lfirst rhs in
                 rfirsts
                 |> Rewriting.Firsts.exists (fun rfirst ->
                        let res, cond = Rewriting.unify lfirst rfirst in
                        if res then (
                          let ctx = Proofctx.clone ctx in
                          ctx |> Proofctx.add_precond cond;
                          let verdict, sub_hist = aux ctx ~first:(lfirst, rfirst) deriv1 deriv2 in
                          hist |> History.add_unfolding sub_hist;
                          verdict)
                        else (
                          hist |> History.add_failure lfirst rfirst;
                          check false)))
        in
        (verdict, false))
    and normalize lhs rhs =
      let lhs =
        lhs
        |> Utils.fixpoint ~f:normalize_trace ~iter:(fun lhs ->
               hist |> History.add_step ("NORMALIZE", (lhs, rhs)))
      in
      let rhs =
        rhs
        |> Utils.fixpoint ~f:normalize_trace ~iter:(fun rhs ->
               hist |> History.add_step ("NORMALIZE", (lhs, rhs)))
      in
      (lhs, rhs)
    in
    (* Verify *)
    let lhs, rhs = normalize lhs rhs in
    let verdict =
      if bot_lhs lhs then (
        hist |> History.add_step ("Bot-LHS", (lhs, rhs));
        check true)
      else if bot_rhs rhs then (
        hist |> History.add_step ("Bot-RHS", (lhs, rhs));
        check false)
      else if disprove lhs rhs then (
        hist |> History.add_step ("DISPROVE", (lhs, rhs));
        check false)
      else if reoccur ctx lhs rhs then (
        hist |> History.add_step ("REOCCUR", (lhs, rhs));
        check true)
      else (
        hist |> History.add_step ("UNFOLD", (lhs, rhs));
        let verdict, terminate = unfold ctx lhs rhs in
        if verdict && terminate then
          check true
        else
          verdict)
    in
    hist |> History.set_verdict verdict;
    (verdict, hist)
  in
  let ctx = Proofctx.make () in
  let lhs, rhs = preprocess ctx lhs rhs in
  let _, tr1 = lhs in
  let _, tr2 = rhs in
  let verdict, entry = aux ctx tr1 tr2 in
  (verdict, ((lhs, rhs), entry))

let verify_entailments (lhs, rhs) =
  let verdict, entries_list =
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
  (verdict, History.from_entries (List.rev entries_list))

let verify_specification (Ast.Spec (entailments, assertion)) =
  let verdict, history = verify_entailments entailments in
  if verdict == assertion then
    (true, Colors.green ^ "Correct" ^ Colors.reset, history)
  else
    ( false,
      Printf.sprintf "%sIncorrect  %sgot: %s%B  %sexpect: %s%B%s" Colors.red Colors.default
        Colors.blue verdict Colors.default Colors.blue assertion Colors.reset,
      history )

let show_verification ~case ~no ~verdict ~verbose ~history =
  let no = string_of_int no in
  Colors.reset
  ^ Printf.sprintf "%s%-10s %s┃  %s\n" Colors.bold ("Case " ^ no) Colors.reset
      (show_specification case)
  ^ Printf.sprintf "%s\n" (History.show history ~verbose)
  ^ Printf.sprintf "%s%-10s %s┃  %s\n" Colors.bold "Verdict" Colors.reset verdict
