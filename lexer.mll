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
  | "<=" | "≤"              { LE }
  | ">"                     { GT }
  | ">=" | "≥"              { GE }
  | "true"                  { TRUTH }
  | "false"                 { FALSENESS }
  | "/\\" | "&&" | "⋀"      { AND }
  | "\\/" | "||" | "⋁"      { OR }
  | "->"  | "→"             { IMPLY }
  | "//"                    { PAR }
  | "#"                     { SHARP }
  | "."  | "·"              { DOT }
  | "^*" | "*" | "﹡"       { KLEENE }
  | "|-" | "=>"             { ENTAIL }
  | ","                     { COMMA }
  | ":"                     { COLON }
  | "::"                    { COLON2 }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "{"                     { LBRACE }
  | "}"                     { RBRACE }
  | "_|_" | "bot"           { BOTTOM }
  | "emp" | "empty"         { EMPTY }
  | "?"                     { QUESTION }
  | digit+ as n             { INT (int_of_string n) }
  | alpha alnum* as id      { IDENT id }
  | _                       { UNKNOWN }
