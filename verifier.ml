type history = {
  first_i : Signals.t;
  first_pi : Ast.pi;
  first_t : Ast.term option;
  mutable iterations : (string * Ast.entailment) list;
  mutable unfoldings : history list;
}

let add_iteration (label, es) hist =
  (* Printf.printf "%s :: %s\n" label (Ast.show_entailment es); *)
  hist.iterations <- (label, es) :: hist.iterations


let add_unfolding sub hist = hist.unfoldings <- sub :: hist.unfoldings

let show_history hist ~verbose =
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
      Printf.sprintf "\027[33m%10s \027[35m│\027[0m  %s%s" name (get_prefix ())
        (Ast.show_entailment entailment)
    in
    let show_first =
      if prefix == "" then
        List.append []
      else
        List.cons
          (Printf.sprintf
             "\027[33m%12s \027[35m│\027[0m  %s\027[35m%s,\027[33m %s,\027[35m %s\027[0m" "∖"
             (get_prefix ()) (Signals.show hist.first_i) (Ast.show_pi hist.first_pi)
             (match hist.first_t with
             | None   -> "_"
             | Some t -> Ast.show_term t))
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


let show_verification ~case ~no ~verdict ~verbose ~history =
  Printf.sprintf "\027[1mCase %-5d :\027[0m  %s\n" no (Ast.show_specification case)
  ^ Printf.sprintf "\027[1mVerify     :\027[0m\n%s\n" (show_history history ~verbose)
  ^ Printf.sprintf "\027[1mVerdict    :\027[0m  %s\n" verdict


let verify_entailment (Ast.Entail { lhs; rhs }) =
  let rec aux ctx (first_i, first_pi, first_t) (lhs : Ast.effects) rhs =
    let hist =
      {
        first_i;
        first_pi = Utils.fixpoint ~f:Ast.normalize_pi first_pi;
        first_t;
        iterations = [];
        unfoldings = [];
      }
    in
    let bot_lhs (_, es1) = es1 = Ast.Bottom
    and bot_rhs (_, es2) = es2 = Ast.Bottom
    and disprove (_, es1) (_, es2) = Inference.nullable es1 && not (Inference.nullable es2)
    and reoccur ctx lhs rhs =
      if lhs = rhs || Proofctx.exists_entail (Entail { lhs; rhs }) ctx then
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
        let ctx = ctx |> Proofctx.add_entail (Entail { lhs; rhs }) in
        firsts
        |> Inference.Set.for_all (fun x ->
               let lhs' = Inference.partial_deriv ctx Proofctx.add_l_imply x lhs in
               let rhs' = Inference.partial_deriv ctx Proofctx.add_r_imply x rhs in
               let verdict, sub_hist = aux ctx x lhs' rhs' in
               hist |> add_unfolding sub_hist;
               verdict)
    and normal lhs rhs =
      let lhs =
        Utils.fixpoint ~f:Ast.normalize
          ~fn_iter:(fun es -> hist |> add_iteration ("NORM-LHS", Entail { lhs = es; rhs }))
          lhs
      in
      let rhs =
        Utils.fixpoint ~f:Ast.normalize
          ~fn_iter:(fun es -> hist |> add_iteration ("NORM-RHS", Entail { lhs; rhs = es }))
          rhs
      in
      (lhs, rhs)
    in
    (* Verify *)
    let lhs, rhs = normal lhs rhs in
    let verdict =
      if bot_lhs lhs then (
        hist |> add_iteration ("Bot-LHS", Entail { lhs; rhs });
        true)
      else if bot_rhs rhs then (
        hist |> add_iteration ("Bot-RHS", Entail { lhs; rhs });
        false)
      else if disprove lhs rhs then (
        hist |> add_iteration ("DISPROVE", Entail { lhs; rhs });
        false)
      else if reoccur ctx lhs rhs then (
        hist |> add_iteration ("REOCCUR", Entail { lhs; rhs });
        true)
      else (
        hist |> add_iteration ("UNFOLD", Entail { lhs; rhs });
        unfold ctx lhs rhs)
    in
    (verdict, hist)
  in
  let ctx = Proofctx.make () in
  let rhs = Ast.disambiguate_effects rhs in
  aux ctx (Signals.empty, True, None) lhs rhs


let verify_specification (Ast.Spec (entailment, assertion)) =
  let verdict, history = verify_entailment entailment in
  if verdict == assertion then
    (true, "\027[32mCorrect\027[0m", history)
  else
    ( false,
      Printf.sprintf "\027[31mIncorrect\027[0m  got: \027[34m%b\027[0m  expect: \027[34m%b\027[0m"
        verdict assertion,
      history )
