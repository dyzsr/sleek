type event = Present of string | Waiting of string

let show_event = function
  | Present name -> name
  | Waiting name -> name ^ "?"
;;

let present name = Present name

let waiting name = Waiting name

let is_waiting = function
  | Waiting _ -> true
  | _         -> false
;;

let is_present = function
  | Present _ -> true
  | _         -> false
;;

let get_present = function
  | Waiting name -> Present name
  | e            -> e
;;

(* Type of signals *)
type t = event list

let show = function
  | []                   -> "{}"
  | [ (Waiting _ as e) ] -> show_event e
  | l                    -> "{" ^ String.concat ", " (List.map show_event l) ^ "}"
;;

(* Null signal *)
let null = []

let is_null = function
  | [] -> true
  | _  -> false
;;

(* Make new signal from name list *)
let make lst = List.sort_uniq compare lst

(* Merge signals `a` and `b` into a new one *)
let merge a b =
  let c = List.sort_uniq compare (a @ b) in
  c |> List.filter (fun x -> is_present x || not (List.exists (( = ) (get_present x)) c))
;;

(* Is `b` included in `a`? *)
let ( |- ) a b =
  b
  |> List.fold_left
       (fun res y ->
         res
         &&
         if is_waiting y then
           a |> List.exists (fun x -> x = y || x = get_present y)
         else
           a |> List.exists (( = ) y))
       true
;;

(* split splits a signal into present part and waiting part *)
let split s = List.partition is_present s

(* Set of signals *)
type set = t list

(* Empty set *)
let empty = []

(* Make a set from single signal *)
let from x = [ x ]

let union a b = a @ b

let forall f s = List.fold_left (fun acc x -> acc && f x) true s

let zip a b = a |> List.fold_left (fun acc x -> acc @ (b |> List.map (fun y -> merge x y))) empty

(* tests *)
let () =
  assert ([] |- []);
  assert ([ present "A" ] |- []);
  assert ([ present "A" ] |- [ present "A" ]);
  assert ([ present "A"; present "B" ] |- [ present "A" ]);
  ()
;;
