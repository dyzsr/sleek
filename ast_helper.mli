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

val total_time_of_es : instants -> term_gen -> term * pi

val visit_pi : (term -> term -> unit) -> pi -> unit
val filter_pi : (term -> term -> bool) -> pi -> pi option
val trim_pi : pi -> term list -> pi

val simplify_term : term -> term
val simplify_pi : pi -> pi
val simplify_es : instants -> instants
val simplify : simple_effects -> simple_effects
