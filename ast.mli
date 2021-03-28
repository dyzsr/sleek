type term =
  | Nil
  | Const of int
  | Var   of string
  | Gen   of int
  | Plus  of term * term
  | Minus of term * term

val ( +* ) : term -> term -> term

val ( -* ) : term -> term -> term

val show_term : term -> string

type atomic_op =
  | Eq
  | Lt
  | Lte
  | Gt
  | Gte

type pi =
  | True
  | False
  | Atomic of atomic_op * term * term
  | And    of pi * pi
  | Or     of pi * pi
  | Imply  of pi * pi
  | Not    of pi

val ( =* ) : term -> term -> pi

val ( >* ) : term -> term -> pi

val ( >=* ) : term -> term -> pi

val ( <* ) : term -> term -> pi

val ( <=* ) : term -> term -> pi

val ( &&* ) : pi -> pi -> pi

val ( ||* ) : pi -> pi -> pi

val ( =>* ) : pi -> pi -> pi

val show_pi : pi -> string

type instants =
  | Bottom
  | Empty
  | Instant  of Signals.t
  | Await    of Signals.event
  | Sequence of instants * instants
  | Union    of instants * instants
  | Parallel of instants * instants
  | Kleene   of instants
  | Timed    of instants * term

val show_instants : instants -> string

type effects = pi * instants

val show_effects : effects -> string

type entailment =
  | Entail of {
      lhs : effects;
      rhs : effects;
    }

val show_entailment : entailment -> string

type specification = Spec of entailment * bool

val show_specification : specification -> string
