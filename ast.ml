type specification = Spec of entailment * bool

and entailment = Entailment of { lhs : effects; rhs : effects }

and effects =
  | Bottom
  | Empty
  | Signal   of Signals.t
  | Sequence of effects * effects
  | Union    of effects * effects
[@@deriving show]
