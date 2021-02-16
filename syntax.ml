exception Syntax_error of string

let pos_to_string Lexing.{ pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum; _ }
    =
  let lstr = "line " ^ string_of_int lnum in
  let cstr = "column " ^ string_of_int (cnum - bol) in
  lstr ^ ", " ^ cstr
;;

let lexer lexbuf =
  let tok = Lexer.lex lexbuf in
  (* let () = print_endline ("token: " ^ token_to_string tok) in *)
  tok
;;

let parse lexer lexbuf =
  try Parser.spec lexer lexbuf
  with Parser.Error ->
    let token = Lexing.lexeme lexbuf in
    let start_p = pos_to_string (Lexing.lexeme_start_p lexbuf) in
    let error_string = start_p ^ ": token '" ^ token ^ "'" in
    raise (Syntax_error error_string)
;;

let parse_from_string str =
  let lexbuf = Lexing.from_string str in
  parse lexer lexbuf
