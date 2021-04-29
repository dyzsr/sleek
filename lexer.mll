{
    open Parser
}

let eol = '\n'
let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let alnum = digit | alpha | '_'

rule lex = parse
  | eol                     { Lexing.new_line lexbuf; lex lexbuf }
  | space                   { lex lexbuf }
  | eof                     { EOF }
  | "True"                  { TRUE }
  | "False"                 { FALSE }
  | "!"                     { EXCLAM }
  | "~"                     { NOT }
  | "+"                     { PLUS }
  | "-"                     { MINUS }
  | "="                     { EQ }
  | "<"                     { LT }
  | "<="                    { LE }
  | ">"                     { GT }
  | ">="                    { GE }
  | "true"                  { TRUTH }
  | "false"                 { FALSENESS }
  | "/\\"                   { AND }
  | "&&"                    { AND }
  | "\\/"                   { OR }
  | "||"                    { OR }
  | "//"                    { PAR }
  | "#"                     { SHARP }
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
  | digit+ as n             { INT (int_of_string n) }
  | lower alnum* as v       { VAR v }
  | upper alnum* as e       { EVENT e }
  | _                       { UNKNOWN }
