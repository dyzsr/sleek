type pure = True | False

val show_pure : pure -> string

type instants =
  | Bottom
  | Empty
  | Instant  of Signals.t
  | Sequence of instants * instants
  | Union    of instants * instants
  | Kleene   of instants

val show_instants : instants -> string

type effects = pure * instants

val show_effects : effects -> string

type entailment = Entailment of { lhs : effects; rhs : effects }

val show_entailment : entailment -> string

type spec = Spec of entailment * bool

val show_spec : spec -> string
