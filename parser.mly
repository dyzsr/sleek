%{
  open Ast
  open Ast_helper
%}

%token EOF              "eof"
%token TRUE             "True"
%token FALSE            "False"
%token TRUTH            "true"
%token FALSENESS        "false"
%token PAR              "//"
%token NOT              "~"
%token EXCLAM           "!"
%token KLEENE           "^*"
%token ENTAIL           "|-"
%token IMPLY            "->"
%token DOT              "."
%token COMMA            ","
%token COLON            ":"
%token COLON2           "::"
%token PLUS             "+"
%token MINUS            "-"
%token AND              "&&"
%token OR               "||"
%token BAR              "|"
%token EQ               "="
%token LT               "<"
%token LE               "<="
%token GT               ">"
%token GE               ">="
%token LPAREN           "("
%token RPAREN           ")"
%token LBRACK           "["
%token RBRACK           "]"
%token LBRACE           "{"
%token RBRACE           "}"
%token BOTTOM           "_|_"
%token EMPTY            "empty"
%token QUESTION         "?"
%token HASH             "#"
%token <float> NUM      "num"
%token <string> IDENT   "ident"

%start specification
%start only_entailments
%start only_entailment
%start only_pitrace
%start only_trace
%start only_instant

%type <Ast.specification> specification
%type <Ast.entailments> only_entailments
%type <Ast.entailment> only_entailment
%type <Ast.pitraces> only_pitrace
%type <Ast.trace> only_trace
%type <Instant.t> only_instant

%right "//"
%nonassoc "#"
%right "->"
%left "||"
%left "&&"
%left "+" "-"
%right "."
%nonassoc "^*"

%%

specification:
    e=entailments ":" a=assertion "eof"   { Spec (e, a) }
  | e=entailments "::" a=assertion "eof"  { Spec (e, a) }

only_entailments:
    e=entailments "eof"                   { e }

only_entailment:
    lhs=pitrace "|-" rhs=pitrace "eof"    { (lhs, rhs) }

only_pitrace:
    l=pitraces "eof"                      { l }

only_trace:
    tr=trace "eof"                        { tr }

only_instant:
    i=instant "eof"                       { i }

assertion:
    "true"                                { true }
  | "false"                               { false }

entailments:
    lhs=pitraces "|-" rhs=pitraces        { (lhs, rhs) }

pitraces:
    e=pitrace                             { [e] }
  | e=pitrace "||" l=pitraces             { e :: l }

pitrace:
    p=pi "&&" tr=trace                    { (p, tr) }
  | p=pi ":"  tr=trace                    { (p, tr) }

pi:
    "(" ")"                               { True }
  | "True"                                { True }
  | "False"                               { False }
  | pi=cmp                                { pi }
  | "~" "(" pi=paren_pi ")"               { Not pi }
  | "(" pi=paren_pi ")"                   { pi }

paren_pi:
  | pi=pi                                 { pi }
  | pi1=paren_pi "&&" pi2=paren_pi        { (pi1 &&* pi2) }
  | pi1=paren_pi "||" pi2=paren_pi        { (pi1 ||* pi2) }
  | pi1=paren_pi "->" pi2=paren_pi        { (pi1 =>* pi2) }

cmp:
    t1=term "=" t2=term                   { (t1 =* t2) }
  | t1=term "<" t2=term                   { (t1 <* t2) }
  | t1=term "<=" t2=term                  { (t1 <=* t2) }
  | t1=term ">" t2=term                   { (t1 >* t2) }
  | t1=term ">=" t2=term                  { (t1 >=* t2) }

term:
    n="num"                               { Const n }
  | v="ident"                             { Var v }
  | t1=term "+" t2=term                   { (t1 +* t2) }
  | t1=term "-" t2=term                   { (t1 -* t2) }

trace:
    "_|_"                                 { Bottom }
  | "empty"                               { Empty }
  | i=instant                             { Instant i }
  | e=waiting                             { Await e }
  | tr1=trace "+" tr2=trace               { Union (tr1, tr2) }
  | tr1=trace "."  tr2=trace              { Sequence (tr1, tr2) }
  | tr1=trace "//" tr2=trace              { Parallel (tr1, tr2) }
  | tr=trace "^*"                         { Kleene tr }
  | "[" ks=pcases "]"                     { PCases ks }
  | "(" tr=trace ")"                      { tr }

pcases:
    k=pcase                               { [ k ] }
  | k=pcase "|" ks=pcases                 { k :: ks }

pcase:
    t=term "->" tr=trace                  { (t, tr) }

instant:
    "{" "}"                               { Instant.empty }
  | "{" l=event_list "}"                  { Instant.make l  }

event_list:
  | e="ident"                             { [ Instant.present e ] }
  | "!" e="ident"                         { [ Instant.absent e ] }
  | e="ident" "," l=event_list            { Instant.present e :: l }
  | "!" e="ident" "," l=event_list        { Instant.absent e :: l }


waiting:
    e="ident" "?"                         { Instant.present e }

%%
