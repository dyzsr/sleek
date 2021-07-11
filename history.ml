open Ast
open Ast_print

type entry = {
  mutable first : first option;
  mutable steps : (string * entail) list;
  mutable children : entry list;
  mutable terms : term list option;
  mutable success : (first list * first list * pi) option;
  mutable failure : (first list * first list * pi) list;
  mutable verdict : bool option;
}

let make_entry () =
  {
    first = None;
    steps = [];
    children = [];
    terms = None;
    success = None;
    failure = [];
    verdict = None;
  }

let add_step (label, entail) ent =
  (* Printf.printf "%s :: %s\n" label (Ast.show_entailment tr); *)
  ent.steps <- (label, entail) :: ent.steps

let add_unfolding child ent = ent.children <- child :: ent.children

let set_first first ent = ent.first <- Some first

let set_terms terms ent = if List.length terms > 0 then ent.terms <- Some terms

let set_success lpath rpath cond ent = ent.success <- Some (lpath, rpath, cond)

let add_failure lpath rpath cond ent = ent.failure <- (lpath, rpath, cond) :: ent.failure

let set_verdict verdict ent = ent.verdict <- Some verdict

let pass = Colors.yellow ^ "✔"
let fail = Colors.red ^ "✘"

let show_entry ent ~verbose =
  let rec aux spaces prefix ent =
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
      let prefix = Colors.default ^ get_prefix () in
      Printf.sprintf "%s%10s %s┃  %s%s%s" Colors.yellow name Colors.magenta prefix message
        Colors.reset
    in
    let show_firsts =
      match ent.first with
      | None       -> id
      | Some first ->
          let verdict =
            match ent.verdict with
            | None         -> ""
            | Some verdict -> if verdict then pass else fail
          in
          List.cons (print ">" (Printf.sprintf "%s : %s" (show_first first) verdict))
    in
    let show_steps =
      if verbose then
        List.append
          [
            (let name, entail = List.hd ent.steps in
             print name (show_entail entail));
            (let name, entail = Utils.last ent.steps in
             print name (show_entail entail));
          ]
      else
        List.cons
          (let name, entail = List.hd ent.steps in
           print name (show_entail entail))
    in
    let show_children =
      List.append
        (ent.children
        |> List.mapi (fun i child ->
               let inner_spaces = if i = 0 then "   " else "│  " in
               let inner_prefix = if i = 0 then "└──" else "├──" in
               let prefix' = get_prefix () in
               let spaces = prefix' ^ inner_spaces in
               let prefix = prefix' ^ inner_prefix in
               aux spaces prefix child))
    in
    let show_terms =
      if verbose then
        match ent.terms with
        | None       -> id
        | Some terms ->
            List.cons
              (print "(TERMS)"
                 (Colors.yellow ^ (List.map show_term terms |> String.concat ", ") ^ Colors.reset))
      else
        id
    in
    let show_paths =
      match ent.success with
      | Some (lpath, rpath, cond) ->
          let lpath = show_path lpath in
          let rpath = show_path rpath in
          List.cons
            (print "+" (Printf.sprintf "%s  %s⊑  %s" lpath Colors.yellow rpath)
            ^ "\n"
            ^ print "$" (Printf.sprintf "%s" (show_pi cond)))
      | None                      ->
          List.append
            (ent.failure
            |> List.map (fun (lpath, rpath, cond) ->
                   let lpath = show_path lpath in
                   let rpath = show_path rpath in
                   print "-" (Printf.sprintf "%s  %s⋢  %s" lpath Colors.yellow rpath)
                   ^ "\n"
                   ^ print "$" (Printf.sprintf "%s" (show_pi cond))))
    in
    [] |> show_firsts |> show_steps |> show_terms |> show_paths |> show_children |> List.rev
    |> String.concat "\n"
  in
  aux "" "" ent

type t = (Ast.entailment * entry) list list

let from_entries l = l

let show hist ~verbose =
  let _, output =
    List.fold_left
      (fun (i, acc) l ->
        ( i + 1,
          let _, entries =
            List.fold_left
              (fun (j, acc2) (entailment, entry) ->
                ( j + 1,
                  let entry = show_entry entry ~verbose in
                  let label =
                    Printf.sprintf "%s%-10s %s┃" Colors.bold (Utils.case_no i j) Colors.no_bold
                  in
                  let case =
                    Printf.sprintf "%s%10s %s┃  %s" Colors.bold "INIT" Colors.no_bold
                      (Colors.underline ^ show_entailment entailment ^ Colors.no_underline)
                  in
                  entry :: case :: label :: acc2 ))
              (1, []) l
          in
          List.rev entries :: acc ))
      (1, []) hist
  in
  String.concat "\n" (List.concat (List.rev output))
