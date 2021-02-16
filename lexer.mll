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
  | eol                 { Lexing.new_line lexbuf; lex lexbuf }
  | space               { lex lexbuf }
  | "True"              { TRUE }
  | "False"             { FALSE }
  | "true"              { TRUTH }
  | "false"             { FALSENESS }
  | "/\\"               { AND }
  | "&&"                { AND }
  | "\\/"               { OR }
  | "||"                { OR }
  | "."                 { DOT }
  | ","                 { COMMA }
  | "^*"                { KLEENE }
  | "|-"                { ENTAIL }
  | ":"                 { COLON }
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
  | "_|_"               { BOTTOM }
  | "emp"               { EMPTY }
  | upper alpha* as e   { EVENT e }
  | eof                 { EOF }
  | _ as l              { raise (Invalid_argument ("character: '" ^ String.make 1 l ^ "'")) }
