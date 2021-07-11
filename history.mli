open Ast

type entry

val show_entry : entry -> verbose:bool -> string
val make_entry : unit -> entry

val add_step : string * entail -> entry -> unit
val add_unfolding : entry -> entry -> unit

val set_first : first -> entry -> unit
val set_terms : term list -> entry -> unit
val set_success : first list -> first list -> pi -> entry -> unit
val add_failure : first list -> first list -> pi -> entry -> unit
val set_verdict : bool -> entry -> unit

type t

val show : t -> verbose:bool -> string

val from_entries : (entailment * entry) list list -> t
