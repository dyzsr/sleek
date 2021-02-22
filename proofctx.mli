type t

val empty : t

val add : Ast.entailment -> t -> t

val exists : Ast.entailment -> t -> bool
