type term =
  | Const of float
  | Var   of string
  | Gen   of int
  | Add   of term * term
  | Sub   of term * term
  | Mul   of term * term
  | Neg   of term

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

type trace =
  | Bottom
  | Empty
  | Instant  of Instant.t
  | Await    of Instant.event
  | Sequence of trace * trace
  | Union    of trace * trace
  | Parallel of trace * trace
  | Kleene   of trace
  | PCases   of (term * trace) list

type first =
  | Null
  | Solid of Instant.t  (** a solid element *)
  | PDist of (term * Instant.t option) list  (** probability distribution *)

type path = first list

type track =
  | SolidTrack of Instant.t list
  | PDistTrack of (term * Instant.t list) list

type effect = pi * trace
type effects = effect list

type entail = trace * trace
type entailment = effect * effect
type entailments = effects * effects

type specification = Spec of entailments * bool
