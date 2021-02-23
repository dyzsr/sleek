let rec nullable : Ast.instants -> bool = function
  | Bottom              -> false
  | Empty               -> true
  | Instant _           -> false
  | Sequence (es1, es2) -> nullable es1 && nullable es2
  | Union (es1, es2)    -> nullable es1 || nullable es2
  | Parallel (es1, es2) -> nullable es1 && nullable es2
  | Kleene _            -> true
;;

let rec first : Ast.instants -> Signals.set = function
  | Bottom -> Signals.empty
  | Empty -> Signals.empty
  | Instant s -> Signals.from s
  | Sequence (es1, es2) when nullable es1 -> Signals.union (first es1) (first es2)
  | Sequence (es1, _) -> first es1
  | Union (es1, es2) -> Signals.union (first es1) (first es2)
  | Parallel (es1, es2) -> Signals.zip (first es1) (first es2)
  | Kleene es -> first es
;;

let rec partial_deriv : Signals.t * Ast.pure -> Ast.effects -> Ast.effects =
 fun (i, lpure) (rpure, rhs) ->
  match rhs with
  | Bottom -> (False, Bottom)
  | Empty -> (False, Bottom)
  | Instant j when Signals.(i |- j) ->
      let j' = Signals.subtract j i in
      if Signals.is_null j' then
        (rpure, Empty)
      else
        (rpure, Instant j')
  | Instant _ -> (False, Bottom)
  | Sequence (es1, es2) when nullable es1 ->
      let _, deriv1 = partial_deriv (i, lpure) (rpure, es1)
      and _, deriv2 = partial_deriv (i, lpure) (rpure, es2) in
      (rpure, Union (Sequence (deriv1, es2), deriv2))
  | Sequence (es1, es2) ->
      let _, deriv1 = partial_deriv (i, lpure) (rpure, es1) in
      (rpure, Sequence (deriv1, es2))
  | Union (es1, es2) ->
      let _, deriv1 = partial_deriv (i, lpure) (rpure, es1)
      and _, deriv2 = partial_deriv (i, lpure) (rpure, es2) in
      (rpure, Union (deriv1, deriv2))
  | Parallel (es1, es2) ->
      let _, deriv1 = partial_deriv (i, lpure) (rpure, es1) in
      let _, deriv2 = partial_deriv (i, lpure) (rpure, es2) in
      (rpure, Parallel (deriv1, deriv2))
  | Kleene es ->
      let _, deriv = partial_deriv (i, lpure) (rpure, es) in
      (rpure, Sequence (deriv, Kleene es))
;;
