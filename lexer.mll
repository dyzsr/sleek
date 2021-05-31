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
  | "#"                     { HASH }
  | "|"                     { BAR }
  | "."  | "·"              { DOT }
  | "^*" | "*"  | "﹡"      { KLEENE }
  | "|-" | "=>" | "⤇"      { ENTAIL }
  | ","                     { COMMA }
  | ":"                     { COLON }
  | "::"                    { COLON2 }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "["                     { LBRACK }
  | "]"                     { RBRACK }
  | "{"                     { LBRACE }
  | "}"                     { RBRACE }
  | "bot" | "_|_" | "⏊"    { BOTTOM }
  | "emp" | "𝝐"             { EMPTY }
  | "?"                     { QUESTION }
  | digit+ as n             { NUM (float_of_string n) }
  | digit+ "." as n         { NUM (float_of_string n) }
  | digit* "." digit+ as n  { NUM (float_of_string n) }
  | alpha alnum* as id      { IDENT id }
