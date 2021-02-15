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

let () =
  List.iter
    (fun case ->
      Printf.printf "Case:      %s\n" (Ast.show_spec case);
      let verdict, history = Verifier.verify_spec case in
      Printf.printf "Verify:\n%s\n" (Verifier.show_history history);
      Printf.printf "Verdict:   %s\n" verdict;
      print_newline ())
    test
;;
