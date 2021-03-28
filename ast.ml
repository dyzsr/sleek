let enclose str = "(" ^ str ^ ")"

let nothing str = str

type term =
  | Nil
  | Const of int
  | Var   of string
  | Gen   of int
  | Plus  of term * term
  | Minus of term * term

let ( +* ) a b = Plus (a, b)

let ( -* ) a b = Minus (a, b)

let rec show_term_with_prec lprec rprec = function
  | Nil            -> "_"
  | Const i        -> string_of_int i
  | Var v          -> v
  | Gen n          -> "t" ^ string_of_int n ^ "'"
  | Plus (t1, t2)  ->
      show_term_with_prec 0 50 t1 ^ "+" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing
  | Minus (t1, t2) ->
      show_term_with_prec 0 50 t1 ^ "-" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing


let show_term p = show_term_with_prec 0 0 p

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

let ( =* ) a b = Atomic (Eq, a, b)

let ( <* ) a b = Atomic (Lt, a, b)

let ( <=* ) a b = Atomic (Lte, a, b)

let ( >* ) a b = Atomic (Gt, a, b)

let ( >=* ) a b = Atomic (Gte, a, b)

let ( &&* ) a b = And (a, b)

let ( ||* ) a b = Or (a, b)

let ( =>* ) a b = Imply (a, b)

let rec show_pi_with_prec lprec rprec = function
  | True                -> "True"
  | False               -> "False"
  | Atomic (op, t1, t2) -> (
      let s1 = show_term t1 in
      let s2 = show_term t2 in
      match op with
      | Eq  -> s1 ^ "=" ^ s2
      | Lt  -> s1 ^ "<" ^ s2
      | Lte -> s1 ^ "≤" ^ s2
      | Gt  -> s1 ^ ">" ^ s2
      | Gte -> s1 ^ "≥" ^ s2)
  | And (p1, p2)        ->
      show_pi_with_prec 0 30 p1 ^ " ⋀ " ^ show_pi_with_prec 30 0 p2
      |> if lprec > 30 || rprec >= 30 then enclose else nothing
  | Or (p1, p2)         ->
      show_pi_with_prec 0 20 p1 ^ " ⋁ " ^ show_pi_with_prec 20 0 p2
      |> if lprec > 20 || rprec >= 20 then enclose else nothing
  | Imply (p1, p2)      ->
      show_pi_with_prec 0 10 p1 ^ " ⇒ " ^ show_pi_with_prec 10 0 p2
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Not p               -> "¬" ^ show_pi_with_prec 90 0 p


let show_pi p = show_pi_with_prec 0 0 p

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

let rec show_instants_with_prec lprec rprec = function
  | Bottom              -> "⏊ "
  | Empty               -> "𝝐"
  | Instant i           -> Signals.show i
  | Await e             -> Signals.show_event e ^ "?"
  | Sequence (es1, es2) ->
      Printf.sprintf "%s·%s" (show_instants_with_prec 0 30 es1) (show_instants_with_prec 30 0 es2)
      |> if lprec > 30 || rprec >= 30 then enclose else nothing
  | Union (es1, es2)    ->
      Printf.sprintf "%s ⋁ %s"
        (show_instants_with_prec 0 20 es1)
        (show_instants_with_prec 20 0 es2)
      |> if lprec > 20 || rprec >= 20 then enclose else nothing
  | Parallel (es1, es2) ->
      Printf.sprintf "%s ‖ %s"
        (show_instants_with_prec 0 10 es1)
        (show_instants_with_prec 10 0 es2)
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Kleene es           ->
      Printf.sprintf "%s﹡" (show_instants_with_prec 0 40 es)
      |> if rprec >= 40 then enclose else nothing
  | Timed (es, term)    ->
      Printf.sprintf "%s # %s" (show_instants_with_prec 0 25 es) (show_term term)
      |> if lprec >= 25 || rprec >= 25 then enclose else nothing


let show_instants es = "\027[36m" ^ show_instants_with_prec 0 0 es ^ "\027[0m"

type effects = pi * instants

let show_effects (pi, instants) =
  Printf.sprintf "%s ⋀ %s" (show_pi_with_prec 0 99 pi) (show_instants instants)


type entailment =
  | Entail of {
      lhs : effects;
      rhs : effects;
    }

let show_entailment (Entail { lhs; rhs }) =
  Printf.sprintf "%s  ├─  %s" (show_effects lhs) (show_effects rhs)


type specification = Spec of entailment * bool

let show_specification (Spec (entailment, assertion)) =
  Printf.sprintf "%s \027[35m: %b\027[0m" (show_entailment entailment) assertion
