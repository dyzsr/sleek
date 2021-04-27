type subhistory = {
  first : (Signals.t * Ast.pi * Ast.term option) option;
  mutable iterations : (string * Ast.simple_entailment) list;
  mutable unfoldings : subhistory list;
}

let add_iteration (label, es) hist =
  (* Printf.printf "%s :: %s\n" label (Ast.show_entailment es); *)
  hist.iterations <- (label, es) :: hist.iterations


let add_unfolding sub hist = hist.unfoldings <- sub :: hist.unfoldings

let show_subhistory hist ~verbose =
  let rec aux spaces prefix hist =
    let first = ref true in
    let get_prefix () =
      if !first then (
        first := false;
        prefix)
      else
        spaces
    in
    let print name entailment =
      Printf.sprintf "%s%10s %s│%s  %s%s" Colors.yellow name Colors.magenta Colors.reset
        (get_prefix ())
        (Ast.show_simple_entailment entailment)
    in
    let show_first =
      match hist.first with
      | None            -> List.append []
      | Some (i, pi, t) ->
          List.cons
            (Printf.sprintf "%s%12s %s│%s  %s%s%s, %s%s, %s%s%s" Colors.yellow "∖"
               Colors.magenta Colors.reset (get_prefix ()) Colors.magenta (Signals.show i)
               Colors.yellow (Ast.show_pi pi) Colors.magenta
               (match t with
               | None   -> "_"
               | Some t -> Ast.show_term t)
               Colors.reset)
    in
    let show_iterations =
      if verbose then
        List.fold_right (fun (name, entailment) acc -> print name entailment :: acc) hist.iterations
      else
        List.cons
          (let name, entailment = List.hd hist.iterations in
           print name entailment)
    in
    let show_unfoldings =
      List.fold_right List.cons
        (List.mapi
           (fun i x ->
             let prefix' = get_prefix () in
             if i = 0 then
               aux (prefix' ^ "   ") (prefix' ^ "└──") x
             else
               aux (prefix' ^ "│  ") (prefix' ^ "├──") x)
           hist.unfoldings)
    in
    [] |> show_first |> show_iterations |> show_unfoldings |> List.rev |> String.concat "\n"
  in
  aux "" "" hist


type history = subhistory list list

let show_history hist ~verbose =
  let _, output =
    List.fold_left
      (fun (i, acc) l ->
        ( i + 1,
          let _, subh =
            List.fold_left
              (fun (j, acc2) sub ->
                ( j + 1,
                  let sub = show_subhistory sub ~verbose in
                  let label =
                    Printf.sprintf "%s%sSub-case %d-%d%s" Colors.cyan Colors.italic i j Colors.reset
                  in
                  sub :: label :: acc2 ))
              (1, []) l
          in
          List.rev subh :: acc ))
      (1, []) hist
  in
  String.concat "\n" (List.concat (List.rev output))


let show_verification ~case ~no ~verdict ~verbose ~history =
  Colors.reset
  ^ Printf.sprintf "%sCase %-5d :%s  %s\n" Colors.bold no Colors.reset (Ast.show_specification case)
  ^ Printf.sprintf "%sVerify     :%s\n%s\n" Colors.bold Colors.reset (show_history history ~verbose)
  ^ Printf.sprintf "%sVerdict    :%s  %s\n" Colors.bold Colors.reset verdict


let verify_simple_entailment (Ast.SimpleEntail { lhs; rhs }) =
  let rec aux ctx first_opt (lhs : Ast.simple_effects) rhs =
    let hist =
      {
        first =
          (match first_opt with
          | None            -> None
          | Some (i, pi, t) -> Some (i, Utils.fixpoint ~f:Ast.normalize_pi pi, t));
        iterations = [];
        unfoldings = [];
      }
    in
    let bot_lhs (_, es1) = es1 = Ast.Bottom
    and bot_rhs (_, es2) = es2 = Ast.Bottom
    and disprove (_, es1) (_, es2) = Inference.nullable es1 && not (Inference.nullable es2)
    and reoccur ctx lhs rhs =
      if lhs = rhs || Proofctx.exists_entail (SimpleEntail { lhs; rhs }) ctx then
        ctx |> Proofctx.check_implies
      else
        false
    and unfold ctx ((pi1, es1) as lhs) ((pi2, _) as rhs) =
      let firsts = Inference.first ctx pi1 es1 in
      if Inference.Set.is_empty firsts then (
        ctx |> Proofctx.add_l_imply ~pre:pi1;
        ctx |> Proofctx.add_r_imply ~pre:pi2;
        ctx |> Proofctx.check_implies)
      else
        let ctx = ctx |> Proofctx.add_entail (SimpleEntail { lhs; rhs }) in
        firsts
        |> Inference.Set.for_all (fun x ->
               let lhs' = Inference.partial_deriv ctx Proofctx.add_l_imply x lhs in
               let rhs' = Inference.partial_deriv ctx Proofctx.add_r_imply x rhs in
               let verdict, sub_hist = aux ctx (Some x) lhs' rhs' in
               hist |> add_unfolding sub_hist;
               verdict)
    and normal lhs rhs =
      let lhs =
        Utils.fixpoint ~f:Ast.normalize
          ~fn_iter:(fun es -> hist |> add_iteration ("NORM-LHS", SimpleEntail { lhs = es; rhs }))
          lhs
      in
      let rhs =
        Utils.fixpoint ~f:Ast.normalize
          ~fn_iter:(fun es -> hist |> add_iteration ("NORM-RHS", SimpleEntail { lhs; rhs = es }))
          rhs
      in
      (lhs, rhs)
    in
    (* Verify *)
    let lhs, rhs = normal lhs rhs in
    let verdict =
      if bot_lhs lhs then (
        hist |> add_iteration ("Bot-LHS", SimpleEntail { lhs; rhs });
        true)
      else if bot_rhs rhs then (
        hist |> add_iteration ("Bot-RHS", SimpleEntail { lhs; rhs });
        false)
      else if disprove lhs rhs then (
        hist |> add_iteration ("DISPROVE", SimpleEntail { lhs; rhs });
        false)
      else if reoccur ctx lhs rhs then (
        hist |> add_iteration ("REOCCUR", SimpleEntail { lhs; rhs });
        true)
      else (
        hist |> add_iteration ("UNFOLD", SimpleEntail { lhs; rhs });
        unfold ctx lhs rhs)
    in
    (verdict, hist)
  in
  let ctx = Proofctx.make () in
  let rhs = Ast.disambiguate_simple_effects rhs in
  aux ctx None lhs rhs


let verify_entailment (Ast.Entail { lhs; rhs }) =
  let verdict, history =
    List.fold_left
      (fun (acc_verdict, acc_history) lhs ->
        if not acc_verdict then
          (false, acc_history)
        else
          let verdict, history =
            List.fold_left
              (fun (acc2_verdict, acc2_history) rhs ->
                if acc2_verdict then
                  (true, acc2_history)
                else
                  let verdict, history = verify_simple_entailment (Ast.SimpleEntail { lhs; rhs }) in
                  (verdict, history :: acc2_history))
              (false, []) rhs
          in
          (verdict, List.rev history :: acc_history))
      (true, []) lhs
  in
  (verdict, List.rev history)


let verify_specification (Ast.Spec (entailment, assertion)) =
  let verdict, history = verify_entailment entailment in
  if verdict == assertion then
    (true, Colors.green ^ "Correct" ^ Colors.reset, history)
  else
    ( false,
      Printf.sprintf "%sIncorrect%s  got: %s%B%s  expect: %s%B%s" Colors.red Colors.reset
        Colors.blue verdict Colors.reset Colors.blue assertion Colors.reset,
      history )
