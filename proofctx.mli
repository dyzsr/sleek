type t

val make : unit -> t

val clone : t -> t

val add_entail : Ast.simple_entailment -> t -> unit

val exists_entail : Ast.simple_entailment -> t -> bool

val new_term : t -> Ast.term

type fn_add_imply = pre:Ast.pi -> ?post:Ast.pi -> t -> unit

val add_precond : Ast.pi -> t -> unit

val add_postcond : Ast.pi -> t -> unit

val check_imply : t -> bool * Ast.pi
