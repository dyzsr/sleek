{
    open Parser
}

let eol = '\n'
let space = [' ' '\t' '\r']
(* let digit = ['0'-'9'] *)
let alpha = ['a'-'z' 'A'-'Z' '_']
let lower = ['a'-'z']
let upper = ['A'-'Z']
(* let alnum = digit | alpha | '_' *)

rule lex = parse
  | eol                     { Lexing.new_line lexbuf; lex lexbuf }
  | space                   { lex lexbuf }
  | eof                     { EOF }
  | "True"                  { TRUE }
  | "False"                 { FALSE }
  | "true"                  { TRUTH }
  | "false"                 { FALSENESS }
  | "/\\"                   { AND }
  | "&&"                    { AND }
  | "\\/"                   { OR }
  | "||"                    { OR }
  | "//"                    { PAR }
  | "."                     { DOT }
  | ","                     { COMMA }
  | "^*"                    { KLEENE }
  | "|-"                    { ENTAIL }
  | ":"                     { COLON }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "{"                     { LBRACE }
  | "}"                     { RBRACE }
  | "_|_"                   { BOTTOM }
  | "bot"                   { BOTTOM }
  | "emp"                   { EMPTY }
  | "?"                     { QUESTION }
  | upper alpha* as e       { EVENT e }
  | _                       { UNKNOWN }
