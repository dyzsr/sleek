open Ast

let enclose str = "(" ^ str ^ ")"
let nothing str = str

let bold str = Colors.bold ^ str ^ Colors.no_bold

let rec show_term_with_prec lprec rprec = function
  | Const n      -> string_of_float n
  | Var v        -> bold v
  | Gen n        -> bold ("v" ^ string_of_int n ^ "'")
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

let show_term p = show_term_with_prec 0 0 p

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
      show_pi_with_prec 0 10 p1 ^ " → " ^ show_pi_with_prec 10 0 p2
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Not p               -> bold "¬" ^ show_pi_with_prec 90 0 p

let show_pi p = Colors.cyan ^ show_pi_with_prec 0 0 p ^ Colors.reset

let rec show_trace_with_prec lprec rprec = function
  | Bottom              -> "⏊ "
  | Empty               -> "𝝐"
  | Instant i           -> Instant.show i
  | Await e             -> Instant.show_event e ^ "?"
  | Sequence (tr1, tr2) ->
      Printf.sprintf "%s·%s" (show_trace_with_prec 0 30 tr1) (show_trace_with_prec 30 0 tr2)
      |> if lprec > 30 || rprec >= 30 then enclose else nothing
  | Union (tr1, tr2)    ->
      Printf.sprintf "%s + %s" (show_trace_with_prec 0 20 tr1) (show_trace_with_prec 20 0 tr2)
      |> if lprec > 20 || rprec >= 20 then enclose else nothing
  | Parallel (tr1, tr2) ->
      Printf.sprintf "%s ║ %s" (show_trace_with_prec 0 10 tr1) (show_trace_with_prec 10 0 tr2)
      |> if lprec > 10 || rprec >= 10 then enclose else nothing
  | Kleene tr           ->
      Printf.sprintf "%s﹡" (show_trace_with_prec 0 40 tr)
      |> if rprec >= 40 then enclose else nothing
  | PCases ks           ->
      let show_case (p, tr) =
        Printf.sprintf "%s → %s" (show_term p) (show_trace_with_prec 0 0 tr)
      in
      List.map show_case ks |> String.concat " | " |> fun x -> "[" ^ x ^ "]"

let show_trace tr = Colors.cyan ^ show_trace_with_prec 0 0 tr ^ Colors.reset

let show_effect (pi, trace) = Printf.sprintf "%s: %s" (show_pi_with_prec 0 99 pi) (show_trace trace)

let show_effects l =
  let strs = List.map show_effect l in
  String.concat (Colors.bold ^ "  ⋁  " ^ Colors.no_bold) strs

let show_entailment (lhs, rhs) = Printf.sprintf "%s  ⤇  %s" (show_effect lhs) (show_effect rhs)

let show_entailments (lhs, rhs) = Printf.sprintf "%s  ⤇  %s" (show_effects lhs) (show_effects rhs)

let show_specification (Spec (entailments, assertion)) =
  Printf.sprintf "%s %s:: %B%s" (show_entailments entailments) Colors.magenta assertion Colors.reset
