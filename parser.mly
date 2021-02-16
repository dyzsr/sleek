%token EOF "eof"
%token TRUE "True" FALSE "False"
%token TRUTH "true" FALSENESS "false"
%token AND "/\\" OR "\\/" DOT "." COMMA ","
%token KLEENE "^*" ENTAIL "|-" COLON ":"
%token LPAREN "(" RPAREN ")"
%token LBRACE "{" RBRACE "}"
%token BOTTOM "_|_" EMPTY "empty"
%token <string> EVENT "event"

%start spec
%type <Ast.spec> spec

%left "\\/"
%left "."
%nonassoc "^*"

%%

spec:
    e=entailment ":" a=assertion "eof" { Ast.Spec (e, a) }

assertion:
    "true"  { true }
  | "false" { false }

entailment:
    lhs=effects "|-" rhs=effects { Ast.Entailment {lhs; rhs} }

effects:
    p=pure "/\\" es=instants { (p, es) }

pure:
    "True"  { Ast.True }
  | "False" { Ast.False }

instants:
    "_|_"                           { Ast.Bottom }
  | "empty"                         { Ast.Empty }
  | i=instant                       { Ast.Instant i }
  | es1=instants "\\/" es2=instants { Ast.Union (es1, es2) }
  | es1=instants "."   es2=instants { Ast.Sequence (es1, es2) }
  | es=instants "^*"                { Ast.Kleene (es) }
  | "(" es=instants ")"             { es }

instant:
    "{" "}"              { Signals.make [] }
  | "{" l=event_list "}" { Signals.make l  }

event_list:
    e="event"                  { [e] }
  | e="event" "," l=event_list { e :: l }

%%
