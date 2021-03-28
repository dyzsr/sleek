type t

val make : unit -> t

val add : Ast.entailment -> t -> t

val exists : Ast.entailment -> t -> bool

val new_term : t -> Ast.term
