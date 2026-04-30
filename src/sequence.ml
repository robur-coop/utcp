(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* arithmetics within the sequence number space -- 0 .. 2 ^ 32 - 1 *)

type t = int

let[@inline always] add a b = (a + b) land 0xFFFFFFFF
let[@inline always] incr a = add a 1
let zero = 0
let[@inline always] addi a i = (add[@inlined]) a i

let[@inline always] sub a b =
  let d = a - b in
  if d < 0 then d + 0x1_0000_0000 else d

let[@inline always] window a b = sub a b

(* project to 0 .. 0xffffffff, i.e. the positive part *)
let of_int32 x =
  let r = Int32.to_int x in
  if x < 0l then r + 0x1_0000_0000 else r

let to_int32 x =
  let r =
    if x >= 0x80000000 then x - 0x1_0000_0000 else x
  in
  Int32.of_int r

external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let[@inline always] less a b = a < b

let[@inline always] less_equal a b = a <= b

let[@inline always] greater a b = a > b

let[@inline always] greater_equal a b = a >= b

let equal a b = a == b

let[@inline always] min a b = if a <= b then a else b
let[@inline always] max a b = if a >= b then a else b

let pp = Fmt.int
