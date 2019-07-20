(* (c) 2019 Hannes Mehnert, all rights reserved *)

let tests = [
  "State machine", State_machine.tests ;
]

let () =
  Printexc.record_backtrace true ;
  Alcotest.run "TCP tests" tests
