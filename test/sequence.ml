open Utcp

(* Tests about sequence number. *)

let max_seq = 0xFFFFFFFFl

let v = Sequence.of_int32
let ffffffff = v max_seq
let z = v 0l
let i32 = Sequence.to_int32

let seqno =
  let pp = Fmt.using i32 Fmt.int32 in
  let equal a b = Int32.equal (i32 a) (i32 b) in
  Alcotest.testable pp equal

let add_wraps_at_2_32 () =
  let v = Sequence.(add ffffffff (v 1l)) in
  Alcotest.(check seqno) "0xFFFFFFFF + 1 = 0" Sequence.zero v

let incr_wraps_at_2_32 () =
  let v = Sequence.incr ffffffff in
  Alcotest.(check seqno) "incr 0xFFFFFFFF = 0" Sequence.zero v

let sub_across_wrap_forward () =
  let v = Sequence.window z ffffffff in
  Alcotest.(check int) "sub 0 0xFFFFFFFF = 1" 1 v

let sub_across_wrap_backward () =
  let v = Sequence.window ffffffff z in
  Alcotest.(check int) "sub 0xFFFFFFFF 0 = 0xffffffff" 0xffffffff v

let window_within_wrap () =
  let snd_una = v 0xFFFFFFF0l and snd_nxt = v 0x10l in
  let v = Sequence.window snd_nxt snd_una in
  Alcotest.(check int) "window across wrap = 0x20" 0x20 v

let window_negative_within_wrap () =
  let snd_una = v 0xFFFFFFF0l and snd_nxt = v 0x10l in
  let v = Sequence.window snd_una snd_nxt in
  Alcotest.(check int) "window across wrap reversed = 0xffffffe0" 0xffffffe0 v

let less_across_wrap () =
  Alcotest.(check bool) "less 0xFFFFFFFF 0 = true" true (Sequence.less ffffffff z);
  Alcotest.(check bool) "less 0 0xFFFFFFFF = false" false (Sequence.less z ffffffff)

let greater_across_wrap () =
  Alcotest.(check bool) "greater 0 0xFFFFFFFF = true" true (Sequence.greater z ffffffff);
  Alcotest.(check bool) "greater 0xFFFFFFFF 0 = false" false (Sequence.greater ffffffff z)

let less_equal_across_wrap () =
  Alcotest.(check bool) "less_equal 0xFFFFFFFF 0 = true" true (Sequence.less_equal ffffffff z);
  Alcotest.(check bool) "less_equal 0xFFFFFFFF 0xFFFFFFFF = true" true (Sequence.less_equal ffffffff ffffffff);
  Alcotest.(check bool) "less_equal 0 0xFFFFFFFF = false" false (Sequence.less_equal z ffffffff)

let greater_equal_across_wrap () =
  Alcotest.(check bool) "greater_equal 0 0xFFFFFFFF = true" true (Sequence.greater_equal z ffffffff);
  Alcotest.(check bool) "greater_equal 0 0 = true" true (Sequence.greater_equal z z);
  Alcotest.(check bool) "greater_equal 0xFFFFFFFF 0 = false" false (Sequence.greater_equal ffffffff z)

let min_across_wrap () =
  Alcotest.(check seqno) "min 0xFFFFFFFF 0 = 0xFFFFFFFF" ffffffff (Sequence.min ffffffff z)

let max_across_wrap () =
  Alcotest.(check seqno) "max 0xFFFFFFFF 0 = 0" z (Sequence.max ffffffff z)

let tests = [
  "add wraps at 2^32",          `Quick, add_wraps_at_2_32 ;
  "incr wraps at 2^32",         `Quick, incr_wraps_at_2_32 ;
  "sub across wrap forward",    `Quick, sub_across_wrap_forward ;
  "sub across wrap backward",   `Quick, sub_across_wrap_backward ;
  "window within wrap",         `Quick, window_within_wrap ;
  "window negative within wrap",`Quick, window_negative_within_wrap ;
  "less across wrap",           `Quick, less_across_wrap ;
  "greater across wrap",        `Quick, greater_across_wrap ;
  "less_equal across wrap",     `Quick, less_equal_across_wrap ;
  "greater_equal across wrap",  `Quick, greater_equal_across_wrap ;
  "min across wrap",            `Quick, min_across_wrap ;
  "max across wrap",            `Quick, max_across_wrap ;
]
