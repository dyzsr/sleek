open Ast
open Ast_helper

module Firsts = struct
  include List

  type t = first list
  let empty = []
  let is_empty t = List.length t = 0
  let singleton i = [ Solid i ]
  let of_list l = List.sort_uniq Stdlib.compare l
  let to_list t = t
  let union a b = a @ b |> List.sort_uniq Stdlib.compare
  let zip a b =
    a
    |> List.fold_left
         (fun acc first1 -> acc @ (b |> List.map (fun first2 -> merge_first first1 first2)))
         empty
    |> List.sort_uniq Stdlib.compare

  module Test = struct
    let test_zip () =
      assert (
        zip (singleton (Parsing.instant "{A}")) (singleton (Parsing.instant "{B}"))
        = singleton (Parsing.instant "{A, B}"));
      assert (
        zip
          (singleton (Parsing.instant "{A}"))
          (singleton (Parsing.instant "{B}") @ singleton (Parsing.instant "{C}"))
        = singleton (Parsing.instant "{A, B}") @ singleton (Parsing.instant "{A, C}"));
      assert (
        zip
          (singleton Instant.empty @ singleton (Parsing.instant "{A}"))
          (singleton Instant.empty @ singleton (Parsing.instant "{B}"))
        = singleton Instant.empty
          @ singleton (Parsing.instant "{A}")
          @ singleton (Parsing.instant "{A, B}")
          @ singleton (Parsing.instant "{B}"));
      ()

    let test () = print_endline "test_zip"; test_zip (); ()
  end
end

open Firsts

let unify first1 first2 =
  match (first1, first2) with
  | Solid i, Solid j   -> (Instant.(i |- j), True)
  | Solid i, PDist d   ->
      let res = List.exists (fun (_, j) -> Instant.(i |- j)) d in
      let p_sum =
        List.fold_left (fun acc (p, j) -> if Instant.(i |- j) then p +* acc else acc) (Const 0.) d
      in
      let cond = p_sum =* Const 1. in
      (res, cond)
  | PDist d, Solid j   ->
      let res = List.exists (fun (_, i) -> Instant.(i |- j)) d in
      let p_sum =
        List.fold_left (fun acc (p, i) -> if Instant.(i |- j) then p +* acc else acc) (Const 0.) d
      in
      let cond = p_sum =* Const 1. in
      (res, cond)
  | PDist d1, PDist d2 ->
      let p2qs =
        d1
        |> List.map (fun (p, i) ->
               let qs =
                 d2
                 |> List.filter_map (fun (q, j) -> if Instant.(i |- j) then Some (q, j) else None)
               in
               (p, qs))
      in
      if List.exists (fun (_, qs) -> qs = []) p2qs then
        (false, False)
      else
        let cond =
          p2qs
          |> List.map (fun (_, qs) ->
                 let p_sum =
                   p2qs
                   |> List.fold_left
                        (fun acc (p', qs') ->
                          if List.for_all (fun (_, j) -> List.exists (fun (_, i) -> i == j) qs) qs'
                          then
                            p' +* acc
                          else
                            acc)
                        (Const 0.)
                 in
                 let q_sum = List.fold_left (fun acc (q, _) -> q +* acc) (Const 0.) qs in
                 p_sum =* q_sum)
          |> List.fold_left ( &&* ) True
        in
        (true, cond)

let first tr =
  let rec aux = function
    | Bottom -> empty
    | Empty -> empty
    | Instant i -> singleton i
    | Await e -> union (singleton Instant.empty) (singleton (Instant.singleton e))
    | Sequence (tr1, tr2) when nullable tr1 -> union (aux tr1) (aux tr2)
    | Sequence (tr1, _) -> aux tr1
    | Union (tr1, tr2) -> union (aux tr1) (aux tr2)
    | Parallel (tr1, tr2) -> zip (aux tr1) (aux tr2)
    | Kleene tr -> aux tr
    | PCases ks ->
        let case2firsts =
          ks
          |> List.map (fun (p, tr) ->
                 let firsts = aux tr |> List.map (fun first -> Some (p, first)) in
                 if nullable tr then None :: firsts else firsts)
        in
        let cases = Utils.combinations case2firsts in
        let firsts =
          cases
          |> List.filter_map (fun case ->
                 let dist =
                   case
                   |> List.fold_left
                        (fun acc -> function
                          | None            -> acc
                          | Some (p, first) -> (
                              match first with
                              | Solid i -> (p, i) :: acc
                              | PDist d -> List.map (fun (in_p, i) -> (p ** in_p, i)) d @ acc))
                        []
                 in
                 if dist = [] then
                   None
                 else
                   Some (PDist dist))
        in
        of_list firsts
  in
  aux tr

let derivative first tr =
  let rec aux ~first tr =
    match tr with
    | Bottom | Empty -> Bottom
    | Instant j -> (
        match first with
        | Solid i -> if Instant.(i |- j) then Empty else Bottom
        | PDist d -> if List.exists (fun (_, i) -> Instant.(i |- j)) d then Empty else Bottom)
    | Await e -> (
        let j = Instant.make [ e ] in
        match first with
        | Solid i -> if Instant.(i |- j) then Empty else Await e
        | PDist d -> if List.exists (fun (_, i) -> Instant.(i |- j)) d then Empty else Await e)
    | Sequence (tr1, tr2) when nullable tr1 ->
        let deriv1 = aux ~first tr1 in
        let deriv2 = aux ~first tr2 in
        Union (Sequence (deriv1, tr2), deriv2)
    | Sequence (tr1, tr2) ->
        let deriv1 = aux ~first tr1 in
        Sequence (deriv1, tr2)
    | Union (tr1, tr2) ->
        let deriv1 = aux ~first tr1 in
        let deriv2 = aux ~first tr2 in
        Union (deriv1, deriv2)
    | Parallel (tr1, tr2) ->
        let deriv1 = aux ~first tr1 in
        let deriv2 = aux ~first tr2 in
        Parallel (deriv1, deriv2)
    | Kleene tr -> aux ~first (Sequence (tr, Kleene tr))
    | PCases ks ->
        let ks =
          ks
          |> List.map (fun (p, tr) -> (p, aux ~first tr))
          |> List.filter (fun (_, tr) -> not (is_bot tr))
        in
        if ks = [] then Bottom else PCases ks
  in
  aux ~first tr
