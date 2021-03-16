type history =
  { first : Signals.t
  ; mutable iterations : (string * Ast.entailment) list
  ; mutable unfoldings : history list
  }

let add_iteration (label, es) hist =
  (* Printf.printf "%s :: %s\n" label (Ast.show_entailment es); *)
  hist.iterations <- (label, es) :: hist.iterations
;;

let add_unfolding sub hist = hist.unfoldings <- sub :: hist.unfoldings

let show_history ~verbose hist =
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
          (Printf.sprintf "\027[33m%12s \027[35m│\027[0m  %s\027[35m%s\027[0m" "∖"
             (get_prefix ()) (Signals.show hist.first))
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
;;

let show_verification ~case ~no ~verdict ~verbose ~history =
  Printf.sprintf "\027[1mCase %-5d :\027[0m  %s\n" no (Ast.show_specification case)
  ^ Printf.sprintf "\027[1mVerify     :\027[0m\n%s\n" (show_history ~verbose history)
  ^ Printf.sprintf "\027[1mVerdict    :\027[0m  %s\n" verdict
;;

let rec normalize_es : Ast.instants -> Ast.instants = function
  | Union (es, Bottom) -> es
  | Union (Bottom, es) -> es
  | Union (es, es') when es = es' -> es
  | Sequence (Empty, es) -> es
  | Sequence (es, Empty) -> es
  | Sequence (Bottom, _) -> Bottom
  | Sequence (_, Bottom) -> Bottom
  | Parallel (es, Empty) -> es
  | Parallel (Empty, es) -> es
  | Parallel (_, Bottom) -> Bottom
  | Parallel (Bottom, _) -> Bottom
  | Parallel (es, es') when es = es' -> es
  | Union (Union (es1, es2), es3) -> Union (es1, Union (es2, es3))
  | Kleene Bottom -> Empty
  | Kleene Empty -> Empty
  | Kleene (Union (Empty, es)) -> Kleene es
  | Sequence (Sequence (es1, es2), es3) -> Sequence (es1, Sequence (es2, es3))
  | Sequence (es, Union (es1, es2)) -> Union (Sequence (es, es1), Sequence (es, es2))
  | Sequence (Union (es1, es2), es) -> Union (Sequence (es1, es), Sequence (es2, es))
  | Parallel (es, Union (es1, es2)) -> Union (Parallel (es, es1), Parallel (es, es2))
  | Parallel (Union (es1, es2), es) -> Union (Parallel (es1, es), Parallel (es2, es))
  (* normalize recursively *)
  | Sequence (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Sequence (es1', es2)
      else
        Sequence (es1, normalize_es es2)
  | Union (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Union (es1', es2)
      else
        Union (es1, normalize_es es2)
  | Parallel (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Parallel (es1', es2)
      else
        Parallel (es1, normalize_es es2)
  | Kleene es -> Kleene (normalize_es es)
  | es -> es
;;

let normalize : Ast.effects -> Ast.effects = function
  | False, _       -> (False, Bottom)
  | pure, instants -> (pure, normalize_es instants)
;;

let verify_entailment (Ast.Entail { lhs; rhs }) =
  let rec aux ctx first lhs rhs =
    let hist = { first; iterations = []; unfoldings = [] } in
    let bot_lhs (_, es1) = es1 = Ast.Bottom
    and bot_rhs (_, es2) = es2 = Ast.Bottom
    and disprove (_, es1) (_, es2) = Inference.nullable es1 && not (Inference.nullable es2)
    and reoccur ctx lhs rhs =
      if lhs = rhs then
        true
      else
        ctx |> Proofctx.exists (Entail { lhs; rhs })
    and unfold ctx ((lpure, es1) as lhs) rhs =
      let firsts = Inference.first es1 in
      let ctx = ctx |> Proofctx.add (Entail { lhs; rhs }) in
      firsts
      |> Signals.forall (fun x ->
             let lhs' = Inference.partial_deriv (x, lpure) lhs in
             let rhs' = Inference.partial_deriv (x, lpure) rhs in
             let verdict, sub_hist = aux ctx x lhs' rhs' in
             hist |> add_unfolding sub_hist;
             verdict)
    and normal lhs rhs =
      let rec iter es =
        let es' = normalize es in
        if es = es' then
          es
        else (
          hist |> add_iteration ("NORM-LHS", Entail { lhs = es; rhs });
          iter es')
      in
      let lhs = iter lhs in
      let rec iter es =
        let es' = normalize es in
        if es = es' then
          es
        else (
          hist |> add_iteration ("NORM-RHS", Entail { lhs; rhs = es });
          iter es')
      in
      let rhs = iter rhs in
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
  aux Proofctx.empty Signals.null lhs rhs
;;

let verify_specification (Ast.Spec (entailment, assertion)) =
  let verdict, history = verify_entailment entailment in
  if verdict == assertion then
    (true, "\027[32mCorrect\027[0m", history)
  else
    ( false
    , Printf.sprintf "\027[31mIncorrect\027[0m  got: \027[34m%b\027[0m  expect: \027[34m%b\027[0m"
        verdict assertion
    , history )
;;
