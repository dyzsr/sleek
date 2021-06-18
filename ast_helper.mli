open Ast

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

type term_gen = int ref

val next_term : term_gen -> term

val visit_pi : (term -> term -> unit) -> pi -> unit
val filter_pi : (term -> term -> bool) -> pi -> pi option
val trim_pi : pi -> term list -> pi

val normalize_term : term -> term
val normalize_pi : pi -> pi
val normalize_trace : trace -> trace
val normalize : pitrace -> pitrace

val amend_constraints : pitrace -> pitrace
