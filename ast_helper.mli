open Ast

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

val is_bot : trace -> bool
val is_null : trace -> bool
val nullable : trace -> bool

val normalize_term : term -> term
val normalize_pi : pi -> pi
val normalize_trace : trace -> trace
val normalize : effect -> effect

val vars_of_term : term -> string list
val terms_of_pi : pi -> term list

val visit_pi : (term -> term -> unit) -> pi -> unit
val filter_pi : (term -> term -> bool) -> pi -> pi option
val trim_pi : pi -> term list -> pi

val amend_constraints : effect -> effect
