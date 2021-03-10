%token UNKNOWN
%token EOF "eof"
%token TRUE "True" FALSE "False"
%token TRUTH "true" FALSENESS "false"
%token AND "&&" OR "||" PAR "//" DOT "." COMMA ","
%token KLEENE "^*" ENTAIL "|-" COLON ":"
%token LPAREN "(" RPAREN ")"
%token LBRACE "{" RBRACE "}"
%token BOTTOM "_|_" EMPTY "empty"
%token QUESTION "?"
%token <string> EVENT "event"

%start specification only_entailment
%type <Ast.specification> specification
%type <Ast.entailment> only_entailment

%right "//"
%right "||"
%right "."
%nonassoc "^*"

%%

specification:
    e=entailment ":" a=assertion "eof" { Ast.Spec (e, a) }

assertion:
    "true"  { true }
  | "false" { false }

only_entailment:
    e=entailment "eof"  { e }

entailment:
    lhs=effects "|-" rhs=effects { Ast.Entail {lhs; rhs} }

effects:
    p=pure "&&" es=instants { (p, es) }

pure:
    "True"  { Ast.True }
  | "False" { Ast.False }

instants:
    "_|_"                           { Ast.Bottom }
  | "empty"                         { Ast.Empty }
  | i=instant                       { Ast.Instant i }
  | i=waiting                       { Ast.Await i }
  | es1=instants "||" es2=instants  { Ast.Union (es1, es2) }
  | es1=instants "."  es2=instants  { Ast.Sequence (es1, es2) }
  | es1=instants "//" es2=instants  { Ast.Parallel (es1, es2) }
  | es=instants "^*"                { Ast.Kleene (es) }
  | "(" es=instants ")"             { es }

instant:
    "{" "}"              { Signals.null }
  | "{" l=event_list "}" { Signals.make l  }

event_list:
    e="event"                  { [ Signals.present e ] }
  | e="event" "," l=event_list { Signals.present e :: l }

waiting:
    e="event" "?"  { Signals.make [ Signals.waiting e ] }

%%
