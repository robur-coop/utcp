(* (c) 2019 Hannes Mehnert, all rights reserved *)

let tests = [
  "Checksum", Checksum.tests ;
  "State machine", State_machine.tests ;
  "Reassembly", Reassembly.tests ;
]

let () =
  Printexc.record_backtrace true ;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level ~all:true (Some Logs.Debug);
  Alcotest.run "TCP tests" tests
