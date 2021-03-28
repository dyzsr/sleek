let error_message text Lexing.{ pos_lnum; pos_bol; pos_cnum; _ } =
  let col = pos_cnum - pos_bol in
  let lines = String.split_on_char '\n' text in
  let rec aux n lines =
    match (n, lines) with
    | 0, lines         -> (String.make col ' ' ^ "^") :: lines
    | n, line :: lines -> line :: aux (n - 1) lines
    | _                -> failwith "invalid position"
  in
  aux pos_lnum lines |> String.concat "\n"


let lexer lexbuf = Lexer.lex lexbuf

let parse parser text =
  let lexbuf = Lexing.from_string text in
  try parser lexer lexbuf
  with _ ->
    let error = error_message text (Lexing.lexeme_start_p lexbuf) in
    Printf.eprintf "\027[31m%s\027[0m\n" error;
    failwith "Syntax error"


let parse_specification text = parse Parser.specification text

let parse_entailment text = parse Parser.only_entailment text
