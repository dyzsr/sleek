type history =
  { mutable iterations : (string * Ast.entailment) list
  ; mutable unfoldings : history list
  }

let show_history hist =
  let rec loop spaces prefix hist =
    let first = ref true in
    let get_prefix () =
      if !first
      then (
        first := false;
        prefix)
      else spaces
    in
    let output =
      []
      |> List.fold_right
           (fun (name, entailment) acc ->
             Printf.sprintf "\027[33m%10s \027[35m│\027[0m  %s%s" name
               (get_prefix ())
               (Ast.show_entailment entailment)
             :: acc)
           hist.iterations
      |> List.fold_right List.cons
           (List.mapi
              (fun i x ->
                let prefix' = get_prefix () in
                if i = 0
                then loop (prefix' ^ "   ") (prefix' ^ "└──") x
                else loop (prefix' ^ "│  ") (prefix' ^ "├──") x)
              hist.unfoldings)
      |> List.rev |> String.concat "\n"
    in
    output
  in
  loop "" "" hist
;;

let add_iteration (label, es) hist =
  (* Printf.printf "%s :: %s\n" label (Ast.show_entailment es); *)
  hist.iterations <- (label, es) :: hist.iterations
;;

let add_unfolding sub hist = hist.unfoldings <- sub :: hist.unfoldings

let rec normalize_es : Ast.instants -> Ast.instants = function
  | Union (es, Bottom) -> es
  | Union (Bottom, es) -> es
  | Union (es, es') when es = es' -> es
  | Sequence (Empty, es) -> es
  | Sequence (es, Empty) -> es
  | Sequence (Bottom, _) -> Bottom
  | Sequence (_, Bottom) -> Bottom
  | Union (Union (es1, es2), es3) -> Union (es1, Union (es2, es3))
  | Kleene Bottom -> Empty
  | Kleene Empty -> Empty
  | Kleene (Union (Empty, es)) -> Kleene es
  | Sequence (Sequence (es1, es2), es3) -> Sequence (es1, Sequence (es2, es3))
  | Sequence (es, Union (es1, es2)) ->
      Union (Sequence (es, es1), Sequence (es, es2))
  | Sequence (Union (es1, es2), es) ->
      Union (Sequence (es1, es), Sequence (es2, es))
  (* normalize recursively *)
  | Union (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1
      then Union (es1', es2)
      else
        let es2' = normalize_es es2 in
        Union (es1, es2')
  | Sequence (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1
      then Sequence (es1', es2)
      else
        let es2' = normalize_es es2 in
        Sequence (es1, es2')
  | Kleene es -> Kleene (normalize_es es)
  | es -> es
;;

let normalization : Ast.effects -> Ast.effects = function
  | False, _ -> (False, Bottom)
  | pure, instants -> (pure, normalize_es instants)
;;

let verify_entailment (Ast.Entailment { lhs; rhs }) =
  let rec iterate ctx lhs rhs =
    let hist = { iterations = []; unfoldings = [] } in
    let bot_lhs (_, lhs) = lhs = Ast.Bottom
    and bot_rhs (_, rhs) = rhs = Ast.Bottom
    and disprove (_, lhs) (_, rhs) =
      Inference.nullable lhs && not (Inference.nullable rhs)
    and prove ctx lhs rhs = ctx |> Proofctx.exists (lhs, rhs)
    and unfold ctx lhs rhs =
      let firsts = Inference.first lhs in
      let ctx = ctx |> Proofctx.add (lhs, rhs) in
      firsts
      |> Inference.forall (fun x ->
             let verdict, sub_hist =
               iterate ctx
                 (Inference.partial_deriv x lhs)
                 (Inference.partial_deriv x rhs)
             in
             hist |> add_unfolding sub_hist;
             verdict)
    and normalize lhs rhs =
      let rec loop es =
        let es' = normalization es in
        if es = es'
        then es
        else (
          hist |> add_iteration ("NORMAL-LHS", Ast.Entailment { lhs = es; rhs });
          loop es')
      in
      let lhs = loop lhs in
      let rec loop es =
        let es' = normalization es in
        if es = es'
        then es
        else (
          hist |> add_iteration ("NORMAL-RHS", Ast.Entailment { lhs; rhs = es });
          loop es')
      in
      let rhs = loop rhs in
      (lhs, rhs)
    in
    (* Verify *)
    let lhs, rhs = normalize lhs rhs in
    let verdict =
      if bot_lhs lhs
      then (
        hist |> add_iteration ("Bot-LHS", Ast.Entailment { lhs; rhs });
        true)
      else if bot_rhs rhs
      then (
        hist |> add_iteration ("Bot-RHS", Ast.Entailment { lhs; rhs });
        false)
      else if disprove lhs rhs
      then (
        hist |> add_iteration ("DISPROVE", Ast.Entailment { lhs; rhs });
        false)
      else if prove ctx lhs rhs
      then (
        hist |> add_iteration ("REOCCUR", Ast.Entailment { lhs; rhs });
        true)
      else (
        hist |> add_iteration ("UNFOLD", Ast.Entailment { lhs; rhs });
        unfold ctx lhs rhs)
    in
    (verdict, hist)
  in
  iterate Proofctx.empty lhs rhs
;;

let verify_spec (Ast.Spec (entailment, assertion)) =
  let verdict, history = verify_entailment entailment in
  if verdict == assertion
  then ("\027[32mCorrect\027[0m", history)
  else
    ( Printf.sprintf
        "\027[31mIncorrect\027[0m  got: \027[34m%b\027[0m  expect: \
         \027[34m%b\027[0m"
        verdict assertion
    , history )
;;
