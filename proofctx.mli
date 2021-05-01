type t

val make : unit -> t

val clone : t -> t

val add_entail : Ast.instants -> Ast.instants -> t -> unit

val exists_entail : Ast.instants -> Ast.instants -> t -> bool

val new_term : t -> Ast.term

val add_precond : Ast.pi -> t -> unit

val add_postcond : Ast.pi -> t -> unit

val track_term : Ast.term -> t -> unit

val tracked_terms : t -> Ast.term list

val check_constraints : t -> bool * Ast.pi
