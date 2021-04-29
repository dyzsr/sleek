let enclose str = "(" ^ str ^ ")"

let nothing str = str

type term =
  | Const of int
  | Var   of string
  | Bar   of string
  | Gen   of int
  | Plus  of term * term
  | Minus of term * term

let ( +* ) a b = Plus (a, b)

let ( -* ) a b = Minus (a, b)

let rec show_term_with_prec lprec rprec = function
  | Const i        -> string_of_int i
  | Var v          -> v
  | Bar v          -> v
  | Gen n          -> "t" ^ string_of_int n ^ "'"
  | Plus (t1, t2)  ->
      show_term_with_prec 0 50 t1 ^ "+" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing
  | Minus (t1, t2) ->
      show_term_with_prec 0 50 t1 ^ "-" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing


let show_term p = Colors.underline ^ show_term_with_prec 0 0 p ^ Colors.no_underline

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

let ( =* ) a b = Atomic (Eq, a, b)

let ( <* ) a b = Atomic (Lt, a, b)

let ( <=* ) a b = Atomic (Le, a, b)

let ( >* ) a b = Atomic (Gt, a, b)

let ( >=* ) a b = Atomic (Ge, a, b)

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
      | Eq -> s1 ^ "=" ^ s2
      | Lt -> s1 ^ "<" ^ s2
      | Le -> s1 ^ "≤" ^ s2
      | Gt -> s1 ^ ">" ^ s2
      | Ge -> s1 ^ "≥" ^ s2)
  | And (p1, p2)        ->
      show_pi_with_prec 0 30 p1 ^ " ⋀ " ^ show_pi_with_prec 30 0 p2
      |> if lprec >= 30 || rprec > 30 then enclose else nothing
  | Or (p1, p2)         ->
      show_pi_with_prec 0 20 p1 ^ " ⋁ " ^ show_pi_with_prec 20 0 p2
      |> if lprec >= 20 || rprec > 20 then enclose else nothing
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


let show_instants es = Colors.cyan ^ show_instants_with_prec 0 0 es ^ Colors.reset

type simple_effects = pi * instants

let show_simple_effects (pi, instants) =
  Printf.sprintf "%s ⋀ %s" (show_pi_with_prec 0 99 pi) (show_instants instants)


type effects = simple_effects list

let show_effects l =
  let strs = List.map show_simple_effects l in
  String.concat (Colors.bold ^ " ⋁ " ^ Colors.no_bold) strs


type simple_entailment =
  | SimpleEntail of {
      lhs : simple_effects;
      rhs : simple_effects;
    }

let show_simple_entailment (SimpleEntail { lhs; rhs }) =
  Printf.sprintf "%s  ├─  %s" (show_simple_effects lhs) (show_simple_effects rhs)


type entailment =
  | Entail of {
      lhs : effects;
      rhs : effects;
    }

let show_entailment (Entail { lhs; rhs }) =
  Printf.sprintf "%s  ├─  %s" (show_effects lhs) (show_effects rhs)


type specification = Spec of entailment * bool

let show_specification (Spec (entailment, assertion)) =
  Printf.sprintf "%s %s: %B%s" (show_entailment entailment) Colors.magenta assertion Colors.reset


let disambiguate_simple_effects (pi, es) =
  let disambiguate_term = function
    | Var v -> Bar (String.capitalize_ascii v)
    | t     -> t
  in
  let rec disambiguate_pi pi =
    match pi with
    | True                -> True
    | False               -> False
    | Atomic (op, t1, t2) -> Atomic (op, disambiguate_term t1, disambiguate_term t2)
    | And (p1, p2)        -> And (disambiguate_pi p1, disambiguate_pi p2)
    | Or (p1, p2)         -> Or (disambiguate_pi p1, disambiguate_pi p2)
    | Imply (p1, p2)      -> Imply (disambiguate_pi p1, disambiguate_pi p2)
    | Not pi              -> Not (disambiguate_pi pi)
  in
  let rec disambiguate_es es =
    match es with
    | Bottom | Empty | Instant _ | Await _ -> es
    | Sequence (es1, es2) -> Sequence (disambiguate_es es1, disambiguate_es es2)
    | Union (es1, es2) -> Union (disambiguate_es es1, disambiguate_es es2)
    | Parallel (es1, es2) -> Parallel (disambiguate_es es1, disambiguate_es es2)
    | Kleene es -> Kleene (disambiguate_es es)
    | Timed (es, t) -> Timed (disambiguate_es es, disambiguate_term t)
  in
  (disambiguate_pi pi, disambiguate_es es)


let rec normalize_pi : pi -> pi = function
  (* reduction *)
  | Atomic (Eq, t1, t2) when t1 = t2 -> True
  | And (True, pi) -> pi
  | And (pi, True) -> pi
  | And (False, _) -> False
  | And (_, False) -> False
  | And (pi, pi') when pi = pi' -> pi
  | Or (True, _) -> True
  | Or (_, True) -> True
  | Or (False, pi) -> pi
  | Or (pi, False) -> pi
  | Or (pi, pi') when pi = pi' -> pi
  | Imply (False, _) -> True
  | Imply (True, pi) -> pi
  | Imply (pi, pi') when pi = pi' -> True
  | Not True -> False
  | Not False -> True
  | And (p1, And (p2, p3)) -> And (And (p1, p2), p3)
  | Or (p1, Or (p2, p3)) -> Or (Or (p1, p2), p3)
  (* normalize recursively *)
  | And (pi1, pi2) ->
      let pi1' = normalize_pi pi1 in
      if pi1' <> pi1 then
        And (pi1', pi2)
      else
        And (pi1, normalize_pi pi2)
  | Or (pi1, pi2) ->
      let pi1' = normalize_pi pi1 in
      if pi1' <> pi1 then
        Or (pi1', pi2)
      else
        Or (pi1, normalize_pi pi2)
  | Not pi -> Not (normalize_pi pi)
  | Imply (pi1, pi2) ->
      let pi1' = normalize_pi pi1 in
      if pi1' <> pi1 then
        Imply (pi1', pi2)
      else
        Imply (pi1, normalize_pi pi2)
  | pi -> pi


let rec normalize_es : instants -> instants = function
  (* reduction *)
  | Union (es, Bottom) -> es
  | Union (Bottom, es) -> es
  | Union (es, es') when es = es' -> es
  | Sequence (Empty, es) -> es
  | Sequence (es, Empty) -> es
  | Sequence (Bottom, _) -> Bottom
  | Sequence (_, Bottom) -> Bottom
  | Parallel (es, Empty) -> es
  | Parallel (Empty, es) -> es
  | Parallel (_, Bottom) -> Bottom
  | Parallel (Bottom, _) -> Bottom
  | Parallel (es, es') when es = es' -> es
  | Union (Union (es1, es2), es3) -> Union (es1, Union (es2, es3))
  | Kleene Bottom -> Empty
  | Kleene Empty -> Empty
  | Kleene (Union (Empty, es)) -> Kleene es
  | Sequence (Sequence (es1, es2), es3) -> Sequence (es1, Sequence (es2, es3))
  (* normalize recursively *)
  | Sequence (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Sequence (es1', es2)
      else
        Sequence (es1, normalize_es es2)
  | Union (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Union (es1', es2)
      else
        Union (es1, normalize_es es2)
  | Parallel (es1, es2) ->
      let es1' = normalize_es es1 in
      if es1' <> es1 then
        Parallel (es1', es2)
      else
        Parallel (es1, normalize_es es2)
  | Kleene es -> Kleene (normalize_es es)
  | Timed (Empty, _) -> Empty
  | Timed (es, t) -> Timed (normalize_es es, t)
  | es -> es


let normalize = function
  | False, _ -> (False, Bottom)
  | pi, es   ->
      let pi' = normalize_pi pi in
      if pi' <> pi then
        (pi', es)
      else
        (pi, normalize_es es)
