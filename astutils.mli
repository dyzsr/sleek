open Ast

(* show ast *)
val show_term : term -> string
val show_pi : pi -> string
val show_trace : trace -> string
val show_first : first -> string
val show_path : path -> string
val show_effect : effect -> string
val show_effects : effects -> string
val show_entail : entail -> string
val show_entailment : entailment -> string
val show_entailments : entailments -> string
val show_specification : specification -> string

(* pi and terms *)
type term_gen = int ref
val next_term : term_gen -> term

val ( +* ) : term -> term -> term
val ( -* ) : term -> term -> term
val ( ** ) : term -> term -> term
val ( =* ) : term -> term -> pi
val ( >* ) : term -> term -> pi
val ( >=* ) : term -> term -> pi
val ( <* ) : term -> term -> pi
val ( <=* ) : term -> term -> pi
val ( &&* ) : pi -> pi -> pi
val ( ||* ) : pi -> pi -> pi
val ( =>* ) : pi -> pi -> pi
val ( !* ) : pi -> pi

val is_const : term -> bool

val vars_of_term : term -> string list
val terms_of_pi : pi -> term list

val visit_pi : (term -> term -> unit) -> pi -> unit
val filter_pi : (term -> term -> bool) -> pi -> pi option
val trim_pi : pi -> term list -> pi

(* trace *)
val is_bot : trace -> bool
val is_null : trace -> bool
val nullable : trace -> bool

(* first and path *)
val merge_first : first -> first -> first
val ( |= ) : first -> first -> bool

(* misc *)
val normalize_term : term -> term
val normalize_pi : pi -> pi
val normalize_trace : trace -> trace
val normalize : effect -> effect

val amend_constraints : effect -> effect
