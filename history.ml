open Ast_print

type entry = {
  mutable first : (Rewriting.first * Rewriting.first) option;
  mutable steps : (string * Ast.entail) list;
  mutable children : (entry, Rewriting.first * Rewriting.first) Either.t list;
  mutable terms : Ast.term list option;
  mutable constraints : Ast.pi option;
  mutable comment : string;
  mutable verdict : bool option;
}

let make_entry () =
  {
    first = None;
    steps = [];
    children = [];
    terms = None;
    constraints = None;
    comment = "";
    verdict = None;
  }

let add_step (label, entail) ent =
  (* Printf.printf "%s :: %s\n" label (Ast.show_entailment tr); *)
  ent.steps <- (label, entail) :: ent.steps

let add_unfolding sub ent = ent.children <- Left sub :: ent.children

let add_failure lfirst rfirst ent = ent.children <- Right (lfirst, rfirst) :: ent.children

let set_first lfirst rfirst ent = ent.first <- Some (lfirst, rfirst)

let set_terms terms ent = if List.length terms > 0 then ent.terms <- Some terms

let set_constraints constrnt ent = ent.constraints <- Some constrnt

let set_comment comment ent = ent.comment <- comment

let set_verdict verdict ent = ent.verdict <- Some verdict

let pass = Colors.yellow ^ "✔"
let reject = Colors.magenta ^ "✘"

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
    let show_first =
      match ent.first with
      | None                  -> id
      | Some (lfirst, rfirst) ->
          let verdict =
            match ent.verdict with
            | None         -> ""
            | Some verdict -> if verdict then pass else reject
          in
          List.cons
            (print "-"
               (Printf.sprintf "%s  %s⊑  %s %s: %s" (Rewriting.show_first lfirst) Colors.yellow
                  (Rewriting.show_first rfirst) Colors.yellow verdict))
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
               match child with
               | Either.Left sub               ->
                   let prefix' = get_prefix () in
                   let spaces = prefix' ^ inner_spaces in
                   let prefix = prefix' ^ inner_prefix in
                   aux spaces prefix sub
               | Either.Right (lfirst, rfirst) ->
                   print "-"
                     (Printf.sprintf "%s%s  %s⋢  %s %s: %s" inner_prefix
                        (Rewriting.show_first lfirst) Colors.yellow (Rewriting.show_first rfirst)
                        Colors.yellow reject)))
    in
    let _show_terms =
      match ent.terms with
      | None       -> id
      | Some terms ->
          List.cons
            (print "(TERMS)"
               (Colors.yellow ^ (List.map show_term terms |> String.concat ", ") ^ Colors.reset))
    in
    let show_constraints =
      match ent.constraints with
      | None      -> id
      | Some True -> id
      | Some con  ->
          let comment = if ent.comment = "" then "" else "\"" ^ ent.comment ^ "\"" in
          List.cons (print "Pi" (show_pi con ^ "  " ^ Colors.cyan ^ comment))
    in
    [] |> show_first |> show_steps |> show_constraints |> show_children |> List.rev
    |> String.concat "\n"
  in
  aux "" "" ent

let roman n =
  assert (n >= 0);
  let digits = [| ""; "I"; "V"; "X"; "L"; "C"; "D"; "M"; "V"; "X"; "L"; "C"; "D"; "M" |] in
  let idx =
    [|
      [];
      [ 1 ];
      [ 1; 1 ];
      [ 1; 1; 1 ];
      [ 1; 2 ];
      [ 2 ];
      [ 2; 1 ];
      [ 2; 1; 1 ];
      [ 2; 1; 1; 1 ];
      [ 1; 3 ];
    |]
  in
  let rec aux p n =
    match (p, n) with
    | 0, 0 -> "O"
    | _, 0 -> ""
    | p, n ->
        let d = n mod 10 in
        let t = List.fold_left (fun acc x -> acc ^ digits.(p + x)) "" idx.(d) in
        aux (p + 2) (n / 10) ^ t
  in
  aux 0 n

let case_no i j =
  assert (i >= 0 && j >= 0);
  let i = roman i in
  Printf.sprintf "%s-%d" i j

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
                    Printf.sprintf "%s%-10s %s┃" Colors.bold (case_no i j) Colors.no_bold
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

let () =
  (* test case_no *)
  assert (case_no 0 0 = "O-0");
  assert (case_no 0 1 = "O-1");
  assert (case_no 1 1 = "I-1");
  assert (case_no 2 1 = "II-1");
  assert (case_no 3 2 = "III-2");
  assert (case_no 4 2 = "IV-2");
  assert (case_no 5 3 = "V-3");
  assert (case_no 6 3 = "VI-3");
  assert (case_no 7 3 = "VII-3");
  assert (case_no 8 3 = "VIII-3");
  assert (case_no 9 3 = "IX-3");
  assert (case_no 10 3 = "X-3");
  assert (case_no 11 3 = "XI-3");
  assert (case_no 12 3 = "XII-3");
  assert (case_no 13 3 = "XIII-3");
  assert (case_no 14 3 = "XIV-3");
  assert (case_no 15 3 = "XV-3");
  assert (case_no 19 3 = "XIX-3");
  assert (case_no 20 3 = "XX-3");
  assert (case_no 40 3 = "XL-3");
  assert (case_no 45 3 = "XLV-3");
  assert (case_no 50 3 = "L-3");
  assert (case_no 60 3 = "LX-3");
  assert (case_no 90 3 = "XC-3");
  assert (case_no 99 3 = "XCIX-3");
  assert (case_no 100 3 = "C-3");
  assert (case_no 200 3 = "CC-3");
  assert (case_no 400 3 = "CD-3");
  assert (case_no 450 3 = "CDL-3");
  assert (case_no 455 3 = "CDLV-3");
  assert (case_no 456 3 = "CDLVI-3");
  assert (case_no 456 3 = "CDLVI-3");
  assert (case_no 495 3 = "CDXCV-3");
  assert (case_no 499 3 = "CDXCIX-3");
  assert (case_no 999 3 = "CMXCIX-3");
  assert (case_no 1000 3 = "M-3");
  assert (case_no 1100 3 = "MC-3");
  assert (case_no 1500 3 = "MD-3");
  assert (case_no 2500 3 = "MMD-3");
  assert (case_no 3999 3 = "MMMCMXCIX-3");
  assert (case_no 4000 3 = "MV-3");
  assert (case_no 9000 3 = "MX-3");
  ()
