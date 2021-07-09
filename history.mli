type entry

val show_entry : entry -> verbose:bool -> string
val make_entry : unit -> entry

val add_step : string * Ast.entail -> entry -> unit
val add_unfolding : entry -> entry -> unit
val add_failure : Rewriting.first -> Rewriting.first -> entry -> unit

val set_first : Rewriting.first -> Rewriting.first -> entry -> unit
val set_terms : Ast.term list -> entry -> unit
val set_constraints : Ast.pi -> entry -> unit
val set_comment : string -> entry -> unit
val set_verdict : bool -> entry -> unit

type t

val show : t -> verbose:bool -> string

val from_entries : (Ast.entailment * entry) list list -> t
