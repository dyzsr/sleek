open Ast

type t

val make : unit -> t

val clone : t -> t

val current_term_gen : t -> Astutils.term_gen
val next_term : t -> term

val add_entail : trace -> trace -> t -> unit
val exists_entail : trace -> trace -> t -> bool

val track_terms : pi -> t -> unit
val tracked_terms : t -> term list

val add_precond : pi -> t -> unit
val add_postcond : pi -> t -> unit

val precond : t -> pi
val postcond : t -> pi

val add_candidates : (first * first) list -> t -> unit
val candidate_combinations : t -> (path * path) list

val fix_effect : effect -> t -> effect

module Test : sig
  val test_entail : unit -> unit
  val test : unit -> unit
end
