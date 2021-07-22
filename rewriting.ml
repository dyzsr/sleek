open Ast
open Astutils

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
  let remove_null t = List.filter (( <> ) Null) t

  module Test = struct
    let test_zip () =
      assert (
        zip (singleton (Parse.instant "{A}")) (singleton (Parse.instant "{B}"))
        = singleton (Parse.instant "{A, B}"));
      assert (
        zip
          (singleton (Parse.instant "{A}"))
          (singleton (Parse.instant "{B}") @ singleton (Parse.instant "{C}"))
        = singleton (Parse.instant "{A, B}") @ singleton (Parse.instant "{A, C}"));
      assert (
        zip
          (singleton Instant.empty @ singleton (Parse.instant "{A}"))
          (singleton Instant.empty @ singleton (Parse.instant "{B}"))
        = singleton Instant.empty
          @ singleton (Parse.instant "{A}")
          @ singleton (Parse.instant "{A, B}")
          @ singleton (Parse.instant "{B}"));
      ()

    let test () = print_endline "test_zip"; test_zip (); ()
  end
end

open Firsts

let unify track1 track2 =
  let ( |- ) is js =
    let rec aux acc is js =
      match (is, js) with
      | [], []           -> acc
      | i :: is, j :: js -> aux (acc && Instant.(i |- j)) is js
      | _                -> false
    in
    aux true is js
  in
  match (track1, track2) with
  | SolidTrack is, SolidTrack js -> if is |- js then (true, True) else (false, False)
  | SolidTrack is, PDistTrack d  ->
      let res = List.exists (fun (_, js) -> is |- js) d in
      let p_sum =
        List.fold_left (fun acc (p, js) -> if is |- js then p +* acc else acc) (Const 0.) d
      in
      (res, p_sum =* Const 1.)
  | PDistTrack d, SolidTrack js  ->
      let res = List.exists (fun (_, is) -> is |- js) d in
      let p_sum =
        List.fold_left (fun acc (p, is) -> if is |- js then p +* acc else acc) (Const 0.) d
      in
      (res, p_sum =* Const 1.)
  | PDistTrack d1, PDistTrack d2 ->
      let p2qs =
        d1
        |> List.map (fun (p, is) ->
               let qs =
                 d2 |> List.filter_map (fun (q, js) -> if is |- js then Some (q, js) else None)
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
                          if
                            qs'
                            |> List.for_all (fun (_, is) ->
                                   qs |> List.exists (fun (_, js) -> is == js))
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
    | Empty -> [ Null ]
    | Instant i -> singleton i
    | Await e -> union (singleton Instant.empty) (singleton (Instant.singleton e))
    | Sequence (tr1, tr2) when nullable tr1 -> union (aux tr1 |> remove_null) (aux tr2)
    | Sequence (tr1, _) -> aux tr1
    | Union (tr1, tr2) -> union (aux tr1) (aux tr2)
    | Parallel (tr1, tr2) -> zip (aux tr1) (aux tr2)
    | Kleene tr -> Null :: aux tr
    | PCases ks ->
        let case2firsts =
          ks
          |> List.map (fun (p, tr) ->
                 let firsts = aux tr |> List.map (fun first -> (p, first)) in
                 (* if nullable tr then (p, None) :: firsts else *)
                 firsts)
        in
        let cases = Utils.combinations case2firsts in
        let firsts =
          cases
          |> List.map (fun case ->
                 let dist =
                   case
                   |> List.fold_left
                        (fun acc -> function
                          | p, Null    -> (p, None) :: acc
                          | p, Solid i -> (p, Some i) :: acc
                          | p, PDist d -> List.map (fun (in_p, i) -> (p ** in_p, i)) d @ acc)
                        []
                 in
                 PDist dist)
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
        | Null    -> Bottom
        | Solid i -> if Instant.(i |- j) then Empty else Bottom
        | PDist d ->
            if
              d
              |> List.exists (function
                   | _, Some i -> Instant.(i |- j)
                   | _, None   -> false)
            then
              Empty
            else
              Bottom)
    | Await e -> (
        let j = Instant.make [ e ] in
        match first with
        | Null    -> Bottom
        | Solid i -> if Instant.(i |- j) then Empty else Await e
        | PDist d ->
            if
              d
              |> List.exists (function
                   | _, Some i -> Instant.(i |- j)
                   | _, None   -> false)
            then
              Empty
            else
              Await e)
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
