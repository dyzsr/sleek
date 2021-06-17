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

val total_time_of_tr : trace -> term_gen -> term * pi

val visit_pi : (term -> term -> unit) -> pi -> unit
val filter_pi : (term -> term -> bool) -> pi -> pi option
val trim_pi : pi -> term list -> pi

val simplify_term : term -> term
val simplify_pi : pi -> pi
val simplify_tr : trace -> trace
val simplify : pitrace -> pitrace

val amend_constraints : pitrace -> pitrace
