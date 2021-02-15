type 'a set = 'a list

let union (a : 'a set) (b : 'a set) = a @ b

let forall (f : 'a -> bool) (s : 'a set) : bool =
  List.fold_left (fun acc x -> acc && f x) true s
;;

let rec nullable : Ast.instants -> bool = function
  | Bottom -> false
  | Empty -> true
  | Instant _ -> false
  | Sequence (es1, es2) -> nullable es1 && nullable es2
  | Union (es1, es2) -> nullable es1 || nullable es2
  | Kleene _ -> true
;;

let rec first : Ast.effects -> (Signals.t * Ast.pure) set = function
  | _, Bottom -> []
  | _, Empty -> []
  | pure, Instant s -> [ (s, pure) ]
  | pure, Sequence (es1, es2) when nullable es1 ->
      union (first (pure, es1)) (first (pure, es2))
  | pure, Sequence (es1, _) -> first (pure, es1)
  | pure, Union (es1, es2) -> union (first (pure, es1)) (first (pure, es2))
  | pure, Kleene es -> first (pure, es)
;;

let rec partial_deriv ((i, lpure) : Signals.t * Ast.pure) :
    Ast.effects -> Ast.effects = function
  | _, Bottom -> (False, Bottom)
  | _, Empty -> (False, Bottom)
  | rpure, Instant j when Signals.(i |- j) -> (rpure, Empty)
  | _, Instant _ -> (False, Bottom)
  | rpure, Sequence (es1, es2) when nullable es1 ->
      let _, deriv1 = partial_deriv (i, lpure) (rpure, es1)
      and _, deriv2 = partial_deriv (i, lpure) (rpure, es2) in
      (rpure, Union (Sequence (deriv1, es2), deriv2))
  | rpure, Sequence (es1, es2) ->
      let _, deriv1 = partial_deriv (i, lpure) (rpure, es1) in
      (rpure, Sequence (deriv1, es2))
  | rpure, Union (es1, es2) ->
      let _, deriv1 = partial_deriv (i, lpure) (rpure, es1)
      and _, deriv2 = partial_deriv (i, lpure) (rpure, es2) in
      (rpure, Union (deriv1, deriv2))
  | rpure, Kleene es ->
      let _, deriv = partial_deriv (i, lpure) (rpure, es) in
      (rpure, Sequence (deriv, Kleene es))
;;
