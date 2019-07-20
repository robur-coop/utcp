(* (c) 2019 Hannes Mehnert, all rights reserved *)

open Tcp

(* generates the bit pattern 0b10100101 *)
let static_rng y =
  let b = Cstruct.create y in Cstruct.memset b 0xA5 ; b

let test_state = Alcotest.testable State.pp State.equal
and test_seg = Alcotest.testable Segment.pp Segment.equal
and test_ip =
  Alcotest.testable Ipaddr.V4.pp (fun a b -> Ipaddr.V4.compare a b = 0)

let test_handle = Alcotest.(pair test_state (option (pair test_ip test_seg)))

(* some setup for testing, they should not be relevant (famous last words) *)
let my_ip = Ipaddr.V4.of_string_exn "1.2.3.4"
and your_ip = Ipaddr.V4.of_string_exn "1.2.3.5"
and listen_port = 1234
and src_port = 4321

let quad = my_ip, listen_port, your_ip, src_port

(* a TCP stack listening on port 1234 *)
let tcp = State.start_listen (State.empty static_rng my_ip) listen_port

let basic_seg = {
  Segment.src_port ; dst_port = listen_port ; seq = Tcp.Sequence.zero ;
  ack = Sequence.zero ; flags = Segment.Flags.empty ; window = 0 ;
  options = [] ; payload = Cstruct.empty
}

(* now we need to put the stack into a TCP state, and push various segments,
   compare whether the return value is what we expect *)
let no_state () =
  (* when the TCP stack does not know the quadruple at all [and there is no
     listener], we expect either drop or RST (never RST a RST) *)
  let dst_port = 1235 in
  let quad = my_ip, dst_port, your_ip, src_port in
  let seg = { basic_seg with dst_port ; seq = Sequence.of_int32 42l } in
  let rst = { basic_seg with
              src_port = seg.dst_port ; dst_port = seg.src_port ;
              flags = Segment.Flags.of_list [`ACK ; `RST ] ; ack = seg.seq } in
  Alcotest.(check  test_handle __LOC__ (tcp, Some (your_ip, rst))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg)) ;
  let seg' = { seg with flags = Segment.Flags.singleton `ACK ; ack = Sequence.of_int32 23l } in
  let rst' = { rst with flags = Segment.Flags.singleton `RST ; ack = Sequence.zero ; seq = seg'.ack } in
  Alcotest.(check  test_handle __LOC__ (tcp, Some (your_ip, rst'))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg')) ;
  let seg'' = { seg with flags = Segment.Flags.singleton `SYN } in
  let rst'' = { rst with ack = Sequence.incr rst.ack } in
  Alcotest.(check  test_handle __LOC__ (tcp, Some (your_ip, rst''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg'')) ;
  let seg''' = { seg with flags = Segment.Flags.singleton `RST } in
  Alcotest.(check  test_handle __LOC__ (tcp, None)
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''')) ;
  let seg'''' = { seg with flags = Segment.Flags.singleton `FIN } in
  let rst''' = { rst with ack = Sequence.incr rst.ack } in
  Alcotest.(check  test_handle __LOC__ (tcp, Some (your_ip, rst'''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg'''')) ;
  let seg''''' = { seg with flags = Segment.Flags.of_list [ `SYN ; `FIN ] } in
  let rst'''' = { rst with ack = Sequence.(incr (incr rst.ack)) } in
  Alcotest.(check  test_handle __LOC__ (tcp, Some (your_ip, rst''''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''''')) ;
  let seg'''''' = { seg with flags = Segment.Flags.singleton `SYN ; payload = Cstruct.create 100 } in
  let rst''''' = { rst with ack = Sequence.(addi rst.ack 101) } in
  Alcotest.(check  test_handle __LOC__ (tcp, Some (your_ip, rst'''''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''''''))
  (* could test more variations of flags / seq&ack, but it's not worth it *)

let tests = [
  "no state", `Quick, no_state
]
