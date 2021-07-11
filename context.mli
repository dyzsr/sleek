open Ast

type t

val make : unit -> t

val clone : t -> t

val current_term_gen : t -> Ast_helper.term_gen
val next_term : t -> term

val replace_constants : effect -> t -> effect

val add_entail : trace -> trace -> t -> unit
val exists_entail : trace -> trace -> t -> bool

val track_terms : pi -> t -> unit
val tracked_terms : t -> term list

val set_precond : pi -> t -> unit
val set_postcond : pi -> t -> unit

val add_precond : pi -> t -> unit
val add_postcond : pi -> t -> unit

val precond : t -> pi
val postcond : t -> pi

val add_candidates : (first * first * pi) list -> t -> unit
val candidate_combinations : t -> (first list * first list * pi) list
