let test =
  let open Ast in
  let open Signals in
  [ Spec
      ( Entailment { lhs = (True, Bottom); rhs = (True, Instant (make [])) }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make [ "A" ]))
          ; rhs = (True, Instant (make []))
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make [ "A" ]))
          ; rhs = (True, Instant (make [ "A" ]))
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make []))
          ; rhs = (True, Instant (make [ "A" ]))
          }
      , false )
  ; Spec
      ( Entailment
          { lhs =
              (True, Sequence (Instant (make [ "A" ]), Instant (make [ "B" ])))
          ; rhs =
              (True, Sequence (Instant (make [ "A" ]), Instant (make [ "B" ])))
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make [ "A" ]))
          ; rhs =
              (True, Sequence (Instant (make [ "A" ]), Instant (make [ "B" ])))
          }
      , false )
  ; Spec
      ( Entailment
          { lhs =
              (True, Sequence (Instant (make [ "A" ]), Instant (make [ "B" ])))
          ; rhs = (True, Instant (make [ "A" ]))
          }
      , false )
  ; Spec
      ( Entailment
          { lhs = (True, Union (Instant (make [ "A" ]), Instant (make [ "B" ])))
          ; rhs = (True, Instant (make [ "A" ]))
          }
      , false )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make [ "A" ]))
          ; rhs = (True, Union (Instant (make [ "A" ]), Instant (make [ "B" ])))
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make [ "A"; "B" ]))
          ; rhs = (True, Instant (make [ "A" ]))
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make [ "A" ]))
          ; rhs = (True, Instant (make [ "A"; "B" ]))
          }
      , false )
  ; Spec
      ( Entailment
          { lhs =
              ( True
              , Union
                  ( Sequence (Instant (make [ "A" ]), Instant (make [ "B" ]))
                  , Sequence (Instant (make [ "C" ]), Instant (make [ "D" ])) )
              )
          ; rhs =
              ( True
              , Union
                  ( Sequence (Instant (make [ "A" ]), Instant (make [ "B" ]))
                  , Sequence (Instant (make [ "C" ]), Instant (make [ "D" ])) )
              )
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Kleene (Instant (make [ "A" ])))
          ; rhs =
              ( True
              , Kleene (Union (Instant (make [ "A" ]), Instant (make [ "B" ])))
              )
          }
      , true )
  ; Spec
      ( Entailment
          { lhs = (True, Instant (make [ "A" ]))
          ; rhs = (True, Kleene (Instant (make [ "A" ])))
          }
      , true )
  ; Spec
      ( Entailment { lhs = (True, Instant (make [ "A" ])); rhs = (True, Bottom) }
      , false )
  ]
;;

let tests =
  List.map Syntax.parse_from_string
    [ "True && {A} |- True && {A} : true"
    ; "True && {}  |- True && {A} : false"
    ; "True && {A} |- True && {}  : true"
    ; "True && {A}.{B} |- True && {A}.{B} : true"
    ; "True && {A} |- True && {A}.{B} : false"
    ; "True && {A}.{B} |- True && {B} : false"
    ; "True && {A}.{B} |- True && {}.{B} : true"
    ; "True && {A}.{B} |- True && {}.{}  : true"
    ; "True && {A} || {B} |- True && {A} : false"
    ; "True && {A} |- True && {A} || {B} : true"
    ; "True && {A} || {B} |- True && {B} || {A} : true"
    ; "True && {A}.{B} || {C}.{D} |- True && {C}.{D} || {A}.{B} : true"
    ; "True && {A}^* |- True && {A}^* : true"
    ; "True && {A}   |- True && {A}^* : true"
    ; "True && {A}^* |- True && {A}   : true"
    ]
;;

let () =
  tests
  |> List.iter (fun case ->
         Printf.printf "\027[1mCase:\027[0m      %s\n" (Ast.show_spec case);
         let verdict, history = Verifier.verify_spec case in
         Printf.printf "\027[1mVerify:\027[0m\n%s\n" (Verifier.show_history history);
         Printf.printf "\027[1mVerdict:\027[0m   %s\n" verdict;
         print_newline ())
;;
