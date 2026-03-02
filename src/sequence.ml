(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* arithmetics within the sequence number space -- 0 .. 2 ^ 32 - 1 *)

type t = int

let[@inline always] add a b = (a + b) land 0xFFFFFFFF
let[@inline always] incr a = add a 1
let zero = 0
let[@inline always] addi a i = (add[@inlined]) a i

let[@inline always] sub a b =
  let d = (a - b) land 0xFFFFFFFF in
  if d land 0x80000000 <> 0 then d - 0x1_0000_0000 else d

let[@inline always] window a b = sub a b

let of_int32 x = Int32.to_int x land 0xFFFFFFFF
let to_int32 = Int32.of_int

let[@inline always] less a b =
  (a - b) land 0x80000000 <> 0

let[@inline always] less_equal a b =
  let d = (a - b) land 0xFFFFFFFF in
  d = 0 || d land 0x80000000 <> 0

let[@inline always] greater a b =
  let d = (a - b) land 0xFFFFFFFF in
  d <> 0 && d land 0x80000000 = 0

let[@inline always] greater_equal a b =
  (a - b) land 0x80000000 = 0

let equal a b = a == b

let[@inline always] min a b = if less a b then a else b
let[@inline always] max a b = if greater a b then a else b

let pp = Fmt.int
