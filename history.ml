type entry = {
  mutable first : Inference.Set.elem option;
  mutable iterations : (string * Ast.simple_entailment) list;
  mutable unfoldings : entry list;
  mutable terms : Ast.term list option;
  mutable constraints : Ast.pi option;
  mutable verdict : bool option;
}

let make_entry () =
  {
    first = None;
    iterations = [];
    unfoldings = [];
    terms = None;
    constraints = None;
    verdict = None;
  }


let set_first (i, t) hist = hist.first <- Some (i, t)

let add_iteration (label, es) hist =
  (* Printf.printf "%s :: %s\n" label (Ast.show_entailment es); *)
  hist.iterations <- (label, es) :: hist.iterations


let add_unfolding sub hist = hist.unfoldings <- sub :: hist.unfoldings

let set_terms terms hist = hist.terms <- Some terms

let set_constraints constrnt hist = hist.constraints <- Some constrnt

let set_verdict verdict hist = hist.verdict <- Some verdict

let show_entry hist ~verbose =
  let rec aux spaces prefix hist =
    let first = ref true in
    let get_prefix () =
      if !first then (
        first := false;
        prefix)
      else
        spaces
    in
    let id x = x in
    let print name message =
      Printf.sprintf "%s%10s %s│%s  %s%s%s" Colors.yellow name Colors.magenta Colors.reset
        (get_prefix ()) message Colors.reset
    in
    let show_first =
      match hist.first with
      | None        -> id
      | Some (i, t) ->
          let first =
            Printf.sprintf "%s%s, %s%s" Colors.magenta (Signals.show i) Colors.yellow
              (match t with
              | None   -> "_"
              | Some t -> Ast.show_term t)
          in
          List.cons (print "-" first)
    in
    let show_iterations =
      if verbose then
        List.fold_right
          (fun (name, entailment) acc -> print name (Ast.show_simple_entailment entailment) :: acc)
          hist.iterations
      else
        List.cons
          (let name, entailment = List.hd hist.iterations in
           print name (Ast.show_simple_entailment entailment))
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
    let show_terms =
      match hist.terms with
      | None       -> id
      | Some terms ->
          List.cons
            (print "|TERMS|"
               (Colors.yellow ^ (List.map Ast.show_term terms |> String.concat ", ") ^ Colors.reset))
    in
    let show_constraints =
      match hist.constraints with
      | None      -> id
      | Some True -> id
      | Some con  -> List.cons (print "|CHECK|" (Ast.show_pi con))
    in
    let show_verdict =
      match hist.verdict with
      | None         -> id
      | Some verdict ->
          List.cons
            (print "|RESULT|"
               (Colors.blue ^ Colors.italic
               ^ (if verdict then "SUCCESS" else "FAILURE")
               ^ Colors.reset))
    in
    [] |> show_first |> show_iterations |> show_unfoldings |> show_terms |> show_constraints
    |> show_verdict |> List.rev |> String.concat "\n"
  in
  aux "" "" hist


type t = entry list list

let from_entries l = l

let show hist ~verbose =
  let _, output =
    List.fold_left
      (fun (i, acc) l ->
        ( i + 1,
          let _, subh =
            List.fold_left
              (fun (j, acc2) sub ->
                ( j + 1,
                  let sub = show_entry sub ~verbose in
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
