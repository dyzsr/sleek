open Ast

let enclose str = "(" ^ str ^ ")"
let nothing str = str

let bold str = Colors.bold ^ str ^ Colors.no_bold

let rec show_term_with_prec lprec rprec = function
  | Const n      -> string_of_float n
  | Var v        -> v
  | Gen n        -> "v" ^ string_of_int n ^ "'"
  | Add (t1, t2) ->
      show_term_with_prec 0 50 t1 ^ "+" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing
  | Sub (t1, t2) ->
      show_term_with_prec 0 50 t1 ^ "-" ^ show_term_with_prec 50 0 t2
      |> if lprec >= 50 || rprec > 50 then enclose else nothing
  | Mul (t1, t2) ->
      show_term_with_prec 0 60 t1 ^ "✕" ^ show_term_with_prec 60 0 t2
      |> if lprec >= 60 || rprec > 60 then enclose else nothing
  | Neg t        -> "-" ^ show_term_with_prec 70 0 t
                    |> if lprec > 0 || rprec > 0 then enclose else nothing

let show_term p = Colors.underline ^ show_term_with_prec 0 0 p ^ Colors.no_underline

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
      show_pi_with_prec 0 30 p1 ^ bold " ⋀ " ^ show_pi_with_prec 30 0 p2
      |> if lprec >= 30 || rprec > 30 then enclose else nothing
  | Or (p1, p2)         ->
      show_pi_with_prec 0 20 p1 ^ bold " ⋁ " ^ show_pi_with_prec 20 0 p2
      |> if lprec >= 20 || rprec > 20 then enclose else nothing
  | Imply (p1, p2)      ->
      show_pi_with_prec 0 10 p1 ^ bold " → " ^ show_pi_with_prec 10 0 p2
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Not p               -> bold "¬" ^ show_pi_with_prec 90 0 p

let show_pi p = Colors.cyan ^ show_pi_with_prec 0 0 p ^ Colors.reset

let rec show_instants_with_prec lprec rprec = function
  | Bottom              -> "⏊ "
  | Empty               -> "𝝐"
  | Instant i           -> Signals.show i
  | Await e             -> Signals.show_event e ^ "?"
  | Sequence (es1, es2) ->
      Printf.sprintf "%s·%s" (show_instants_with_prec 0 30 es1) (show_instants_with_prec 30 0 es2)
      |> if lprec > 30 || rprec >= 30 then enclose else nothing
  | Union (es1, es2)    ->
      Printf.sprintf "%s + %s" (show_instants_with_prec 0 20 es1) (show_instants_with_prec 20 0 es2)
      |> if lprec > 20 || rprec >= 20 then enclose else nothing
  | Parallel (es1, es2) ->
      Printf.sprintf "%s ║ %s"
        (show_instants_with_prec 0 10 es1)
        (show_instants_with_prec 10 0 es2)
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Kleene es           ->
      Printf.sprintf "%s﹡" (show_instants_with_prec 0 40 es)
      |> if rprec >= 40 then enclose else nothing
  | Timed (es, term)    ->
      Printf.sprintf "%s # %s" (show_instants_with_prec 0 20 es) (show_term term)
      |> if lprec >= 20 || rprec >= 20 then enclose else nothing
  | PCases ks           ->
      let show_case (p, es) =
        Printf.sprintf "%s → %s" (show_term p) (show_instants_with_prec 0 0 es)
      in
      List.map show_case ks |> String.concat " | " |> fun x -> "[" ^ x ^ "]"

let show_instants es = Colors.cyan ^ show_instants_with_prec 0 0 es ^ Colors.reset

let show_simple_effects (pi, instants) =
  Printf.sprintf "%s: %s" (show_pi_with_prec 0 99 pi) (show_instants instants)

let show_effects l =
  let strs = List.map show_simple_effects l in
  String.concat (Colors.bold ^ "  ⋁  " ^ Colors.no_bold) strs

let show_simple_entailment (SimpleEntail { lhs; rhs }) =
  Printf.sprintf "%s  ⤇  %s" (show_simple_effects lhs) (show_simple_effects rhs)

let show_entailment (Entail { lhs; rhs }) =
  Printf.sprintf "%s  ⤇  %s" (show_effects lhs) (show_effects rhs)

let show_specification (Spec (entailment, assertion)) =
  Printf.sprintf "%s %s:: %B%s" (show_entailment entailment) Colors.magenta assertion Colors.reset
