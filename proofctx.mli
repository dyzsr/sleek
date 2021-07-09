type t

val make : unit -> t

val clone : t -> t

val current_term_gen : t -> Ast_helper.term_gen
val next_term : t -> Ast.term

val replace_constants : Ast.effect -> t -> Ast.effect

val add_entail : Ast.trace -> Ast.trace -> t -> unit
val exists_entail : Ast.trace -> Ast.trace -> t -> bool

val set_precond : Ast.pi -> t -> unit
val set_postcond : Ast.pi -> t -> unit

val add_precond : Ast.pi -> t -> unit
val add_postcond : Ast.pi -> t -> unit

val tracked_terms : t -> Ast.term list
val constraints : t -> Ast.pi
val check_constraints : t -> bool * string
