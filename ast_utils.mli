open Ast

val ( +* ) : term -> term -> term

val ( -* ) : term -> term -> term

val ( =* ) : term -> term -> pi

val ( >* ) : term -> term -> pi

val ( >=* ) : term -> term -> pi

val ( <* ) : term -> term -> pi

val ( <=* ) : term -> term -> pi

val ( &&* ) : pi -> pi -> pi

val ( ||* ) : pi -> pi -> pi

val ( =>* ) : pi -> pi -> pi

val ( !* ) : pi -> pi

val trim_constraints : pi -> term list -> pi

val trim_simple_effects : simple_effects -> simple_effects

val disambiguate_simple_effects : simple_effects -> simple_effects

val normalize_pi : pi -> pi

val normalize_es : instants -> instants

val normalize : simple_effects -> simple_effects
