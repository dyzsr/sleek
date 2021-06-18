open Ast_helper
open Ast_print

module Set = struct
  include List

  type first =
    | Base of Instant.t  (** base case *)
    | Dist of (Ast.term * Instant.t) list  (** probability distribution *)

  let show_first = function
    | Base i  -> Colors.magenta ^ Instant.show i ^ Colors.reset
    | Dist ks ->
        Colors.yellow ^ "{"
        ^ String.concat " | "
            (List.map
               (fun (p, i) ->
                 Colors.magenta
                 ^ Printf.sprintf "%s %s→%s " (show_term p) Colors.yellow Colors.magenta
                 ^ Instant.show i ^ Colors.yellow)
               ks)
        ^ "}" ^ Colors.reset

  type t = first list
  let empty = []
  let is_empty s = List.length s = 0
  let singleton i = [ Base i ]
  let union a b = a @ b |> List.sort_uniq Stdlib.compare
  let zip a b =
    a
    |> List.fold_left
         (fun acc elm1 ->
           acc
           @ (b
             |> List.map (fun elm2 ->
                    match (elm1, elm2) with
                    | Base i1, Base i2                  -> Base (Instant.merge i1 i2)
                    | Dist ks, Base i | Base i, Dist ks ->
                        Dist (List.map (fun (p, i') -> (p, Instant.merge i i')) ks)
                    | Dist ks1, Dist ks2                ->
                        Dist
                          (ks1
                          |> List.concat_map (fun (p1, i1) ->
                                 ks2 |> List.map (fun (p2, i2) -> (p1 ** p2, Instant.merge i1 i2)))
                          ))))
         empty
    |> List.sort_uniq Stdlib.compare

  let () =
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
end

open Ast
open Set

let rec is_bot = function
  | Bottom              -> true
  | Empty               -> false
  | Instant _           -> false
  | Await _             -> false
  | Sequence (tr1, tr2) -> is_bot tr1 || is_bot tr2
  | Union (tr1, tr2)    -> is_bot tr1 && is_bot tr2
  | Parallel (tr1, tr2) -> is_bot tr1 || is_bot tr2
  | Kleene _            -> false
  | PCases ks           -> List.fold_left (fun acc (_, tr) -> acc && is_bot tr) true ks

let rec nullable = function
  | Bottom              -> false
  | Empty               -> true
  | Instant _           -> false
  | Await _             -> false
  | Sequence (tr1, tr2) -> nullable tr1 && nullable tr2
  | Union (tr1, tr2)    -> nullable tr1 || nullable tr2
  | Parallel (tr1, tr2) -> nullable tr1 && nullable tr2
  | Kleene _            -> true
  | PCases ks           -> List.fold_left (fun acc (_, tr) -> acc || nullable tr) false ks

let first ctx tr =
  let rec aux = function
    | Bottom -> empty
    | Empty -> empty
    | Instant i -> singleton i
    | Await e -> union (singleton Instant.empty) (singleton (Instant.make [ e ]))
    | Sequence (tr1, tr2) when nullable tr1 -> union (aux tr1) (aux tr2)
    | Sequence (tr1, _) -> aux tr1
    | Union (tr1, tr2) -> union (aux tr1) (aux tr2)
    | Parallel (tr1, tr2) -> zip (aux tr1) (aux tr2)
    | Kleene tr -> aux tr
    | PCases ks ->
        let elmss = List.map (fun (_, tr) -> aux tr) ks in
        let kss = Utils.combinations elmss in
        let elms =
          kss
          |> List.map (fun ks ->
                 Dist
                   (List.fold_left
                      (fun acc elm ->
                        match elm with
                        | Base i     ->
                            let p = ctx |> Proofctx.next_term in
                            (p, i) :: acc
                        | Dist in_ks -> in_ks @ acc)
                      [] ks))
        in
        elms
  in
  aux tr

let partial_deriv ctx first tr =
  let rec aux ~first ?p tr =
    match tr with
    | Bottom -> Bottom
    | Empty -> Bottom
    | Instant j -> (
        match first with
        | Base i   -> if Instant.(i |- j) then Empty else Bottom
        | Dist ks' -> (
            match p with
            | Some _ -> failwith "impossible"
            | None   ->
                if List.for_all (fun (_, i) -> Instant.(i |- j)) ks' then (
                  let p' = ks' |> List.fold_left (fun acc (p', _) -> p' +* acc) (Const 0.) in
                  ctx |> Proofctx.track_term p';
                  ctx |> Proofctx.add_precond (p' =* Const 1.);
                  Empty)
                else
                  Bottom))
    | Await e -> (
        let j = Instant.make [ e ] in
        match first with
        | Base i   -> if Instant.(i |- j) then Empty else Await e
        | Dist ks' -> (
            match p with
            | Some _ -> failwith "impossible"
            | _      ->
                if List.for_all (fun (_, i) -> Instant.(i |- j)) ks' then (
                  let p' = ks' |> List.fold_left (fun acc (p', _) -> p' +* acc) (Const 0.) in
                  ctx |> Proofctx.track_term p';
                  ctx |> Proofctx.add_precond (p' =* Const 1.);
                  Empty)
                else if List.exists (fun (_, i) -> Instant.(i |- j)) ks' then
                  Bottom
                else
                  Await e))
    | Sequence (tr1, tr2) when nullable tr1 ->
        let deriv1 = aux ~first tr1 ?p in
        let deriv2 = aux ~first tr2 ?p in
        Union (Sequence (deriv1, tr2), deriv2)
    | Sequence (tr1, tr2) ->
        let deriv1 = aux ~first tr1 ?p in
        Sequence (deriv1, tr2)
    | Union (tr1, tr2) ->
        let deriv1 = aux ~first tr1 ?p in
        let deriv2 = aux ~first tr2 ?p in
        Union (deriv1, deriv2)
    | Parallel (tr1, tr2) ->
        let deriv1 = aux ~first tr1 ?p in
        let deriv2 = aux ~first tr2 ?p in
        Parallel (deriv1, deriv2)
    | Kleene tr -> aux ~first (Sequence (tr, Kleene tr)) ?p
    | PCases ks -> (
        match first with
        | Base _   ->
            let derivs =
              List.map
                (fun (in_p, tr) ->
                  match p with
                  | Some p -> (in_p, aux ~first tr ~p:(p ** in_p))
                  | None   -> (in_p, aux ~first tr ~p:in_p))
                ks
            in
            derivs
            |> List.iter (fun (in_p, tr) ->
                   if is_bot tr then (
                     ctx |> Proofctx.track_term in_p;
                     ctx |> Proofctx.add_precond (in_p =* Const 0.)));
            PCases derivs
        | Dist ks' ->
            let derivss =
              ks'
              |> List.map (fun (_, i) ->
                     ks
                     |> List.map (fun (in_p, tr) ->
                            match p with
                            | Some p -> aux ~first:(Base i) tr ~p:(p ** in_p)
                            | None   -> aux ~first:(Base i) tr ~p:in_p))
            in
            if List.exists (fun derivs -> List.for_all is_bot derivs) derivss then
              Bottom
            else
              let derivss = Utils.zip derivss in
              let derivs =
                List.map
                  (fun candidates ->
                    match List.find_opt (fun deriv -> not (is_bot deriv)) candidates with
                    | None       -> Bottom
                    | Some deriv -> deriv)
                  derivss
              in
              let derivs = List.map2 (fun (in_p, _) deriv -> (in_p, deriv)) ks derivs in
              PCases derivs)
  in
  aux ~first tr
