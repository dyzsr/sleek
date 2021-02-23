type pure = True | False

let show_pure = function True -> "True" | False -> "False"

type instants =
  | Bottom
  | Empty
  | Instant  of Signals.t
  | Sequence of instants * instants
  | Union    of instants * instants
  | Parallel of instants * instants
  | Kleene   of instants

let show_instants es =
  let enclose str = "(" ^ str ^ ")" in
  let nothing str = str in
  let rec aux lprec rprec = function
    | Bottom              -> "⏊ "
    | Empty               -> "𝝐"
    | Instant i           -> Signals.show i
    | Sequence (es1, es2) ->
        Printf.sprintf "%s · %s" (aux 0 30 es1) (aux 30 0 es2)
        |> if lprec > 30 || rprec >= 30 then enclose else nothing
    | Union (es1, es2)    ->
        Printf.sprintf "%s ⋁ %s" (aux 0 20 es1) (aux 20 0 es2)
        |> if lprec > 20 || rprec >= 20 then enclose else nothing
    | Parallel (es1, es2) ->
        Printf.sprintf "%s ‖ %s" (aux 0 10 es1) (aux 10 0 es2)
        |> if lprec > 10 || rprec >= 10 then enclose else nothing
    | Kleene es           -> Printf.sprintf "%s^*" (aux 0 40 es)
                             |> if rprec >= 40 then enclose else nothing
  in
  "\027[36m" ^ aux 0 0 es ^ "\027[0m"
;;

type effects = pure * instants

let show_effects (pure, instants) =
  Printf.sprintf "%s ⋀  %s" (show_pure pure) (show_instants instants)
;;

type entailment = Entail of { lhs : effects; rhs : effects }

let show_entailment (Entail { lhs; rhs }) =
  Printf.sprintf "%s  ⊑  %s" (show_effects lhs) (show_effects rhs)
;;

type specification = Spec of entailment * bool

let show_specification (Spec (entailment, assertion)) =
  Printf.sprintf "%s \027[35m: %b\027[0m" (show_entailment entailment) assertion
;;
