type term =
  | Const of float
  | Var   of string
  | Gen   of int
  | Add   of term * term
  | Sub   of term * term
  | Mul   of term * term

type atomic_op =
  | Eq
  | Lt
  | Le
  | Gt
  | Ge

type pi =
  | True
  | False
  | Atomic of atomic_op * term * term
  | And    of pi * pi
  | Or     of pi * pi
  | Imply  of pi * pi
  | Not    of pi

type instants =
  | Bottom
  | Empty
  | Instant  of Signals.t
  | Await    of Signals.event
  | Sequence of instants * instants
  | Union    of instants * instants
  | Parallel of instants * instants
  | PCases   of (term * instants) list
  | Kleene   of instants
  | Timed    of instants * term

type simple_effects = pi * instants

type effects = simple_effects list

type simple_entailment =
  | SimpleEntail of {
      lhs : simple_effects;
      rhs : simple_effects;
    }

type entailment =
  | Entail of {
      lhs : effects;
      rhs : effects;
    }

type specification = Spec of entailment * bool
