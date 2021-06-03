let fixpoint ~f ?(fn_iter = fun _ -> ()) ?(fn_stop = fun _ -> ()) init =
  let rec iter cur =
    let next = f cur in
    if cur = next then (
      fn_stop cur; cur)
    else (
      fn_iter cur; iter next)
  in
  iter init

let opt_value = function
  | None   -> raise (Invalid_argument "none option")
  | Some x -> x

let opt_iter ~f = function
  | None   -> ()
  | Some x -> f x

let opt_map ~f = function
  | None   -> None
  | Some x -> Some (f x)

let opt_map2 ?(a = fun x -> x) ?(b = fun y -> y) ~ab x y =
  match (x, y) with
  | None, None     -> None
  | Some x, None   -> Some (a x)
  | None, Some y   -> Some (b y)
  | Some x, Some y -> Some (ab x y)
