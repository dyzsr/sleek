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

%start specification only_entailment
%start only_effects only_instants
%start simple_entailment
%type <Ast.specification> specification
%type <Ast.entailment> only_entailment
%type <Ast.effects> only_effects
%type <Ast.instants> only_instants
%type <Ast.simple_entailment> simple_entailment

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
    e=entailment ":" a=assertion "eof"  { Ast.Spec (e, a) }
  | e=entailment "::" a=assertion "eof" { Ast.Spec (e, a) }

only_entailment:
    e=entailment "eof"  { e }

only_effects:
    l=effects "eof"     { l }

only_instants:
    es=instants "eof"   { es }

simple_entailment:
    lhs=simple_effects "|-" rhs=simple_effects "eof"  { Ast.SimpleEntail {lhs; rhs} }

assertion:
    "true"              { true }
  | "false"             { false }

entailment:
    lhs=effects "|-" rhs=effects      { Ast.Entail {lhs; rhs} }

effects:
    e=simple_effects                  { [e] }
  | e=simple_effects "||" l=effects   { e :: l }

simple_effects:
    p=pi "&&" es=instants             { (p, es) }
  | p=pi ":"  es=instants             { (p, es) }

pi:
    "(" ")"                           { Ast.True }
  | "True"                            { Ast.True }
  | "False"                           { Ast.False }
  | pi=cmp                            { pi }
  | "~" "(" pi=paren_pi ")"           { Ast.Not pi }
  | "(" pi=paren_pi ")"               { pi }

paren_pi:
  | pi=pi                             { pi }
  | pi1=paren_pi "&&" pi2=paren_pi    { Ast_helper.(pi1 &&* pi2) }
  | pi1=paren_pi "||" pi2=paren_pi    { Ast_helper.(pi1 ||* pi2) }
  | pi1=paren_pi "->" pi2=paren_pi    { Ast_helper.(pi1 =>* pi2) }

cmp:
    t1=term "=" t2=term               { Ast_helper.(t1 =* t2) }
  | t1=term "<" t2=term               { Ast_helper.(t1 <* t2) }
  | t1=term "<=" t2=term              { Ast_helper.(t1 <=* t2) }
  | t1=term ">" t2=term               { Ast_helper.(t1 >* t2) }
  | t1=term ">=" t2=term              { Ast_helper.(t1 >=* t2) }

term:
    n="num"                           { Ast.Const n }
  | v="ident"                         { Ast.Var v }
  | t1=term "+" t2=term               { Ast_helper.(t1 +* t2) }
  | t1=term "-" t2=term               { Ast_helper.(t1 -* t2) }

instants:
    "_|_"                             { Ast.Bottom }
  | "empty"                           { Ast.Empty }
  | i=instant                         { Ast.Instant i }
  | e=waiting                         { Ast.Await e }
  | es1=instants "+" es2=instants     { Ast.Union (es1, es2) }
  | es1=instants "."  es2=instants    { Ast.Sequence (es1, es2) }
  | es1=instants "//" es2=instants    { Ast.Parallel (es1, es2) }
  | "[" ks=pcases "]"                 { Ast.PCases ks }
  | es=instants "^*"                  { Ast.Kleene (es) }
  | es=instants "#" t=term            { Ast.Timed (es, t) }
  | "(" es=instants ")"               { es }

pcases:
    k=pcase                           { [ k ] }
  | k=pcase "|" ks=pcases             { k :: ks }

pcase:
    t=term "->" es=instants           { (t, es) }

instant:
    "{" "}"                           { Signals.empty }
  | "{" l=event_list "}"              { Signals.make l  }

event_list:
  | e="ident"                         { [ Signals.present e ] }
  | "!" e="ident"                     { [ Signals.absent e ] }
  | e="ident" "," l=event_list        { Signals.present e :: l }
  | "!" e="ident" "," l=event_list    { Signals.absent e :: l }


waiting:
    e="ident" "?"                     { Signals.present e }

%%
