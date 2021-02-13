type 'a t = ('a * 'a) list

let empty : 'a t = []

let add (a, b) (ctx : 'a t) : 'a t =
  (a, b)
  :: List.map
       (fun x -> (a, x))
       (List.map snd (List.filter (fun (x, _) -> b = x) ctx))

let exists v (ctx : 'a t) = List.exists (( = ) v) ctx
