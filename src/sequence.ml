(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* arithmetics within the sequence number space -- 0 .. 2 ^ 32 - 1 *)

type t = int

let[@inline always] add a b = (a + b) land 0xFFFFFFFF
let[@inline always] incr a = add a 1
let zero = 0
let[@inline always] addi a i = (add[@inlined]) a i

external sub : t -> t -> int = "%subint"
external window : t -> t -> int = "%subint"

let of_int32 x = Int32.to_int x land 0xFFFFFFFF
let to_int32 = Int32.of_int

external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let ( > ) (x : int) y = x > y [@@inline]
let ( < ) (x : int) y = x < y [@@inline]
let ( <= ) (x : int) y = x <= y [@@inline]
let ( >= ) (x : int) y = x >= y [@@inline]
let min (a : int) b = if a <= b then a else b [@@inline]
let max (a : int) b = if a >= b then a else b [@@inline]

let less a b = a - b < 0
let less_equal a b = a - b <= 0
let greater a b = a - b > 0
let greater_equal a b = a - b >= 0
let equal a b = a == b

let pp = Fmt.int
