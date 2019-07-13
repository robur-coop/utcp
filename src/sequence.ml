(* (c) 2017 Hannes Mehnert, all rights reserved *)

(* arithmetics within the sequence number space -- 0 .. 2 ^ 32 - 1 *)

(* TODO: evaluate performance, maybe int is more pleasant *)
type t = int32

let add = Int32.add
let incr a = add a 1l

let zero = 0l

let window a b = Int32.to_int (Int32.sub a b)

let addi a i = add a (Int32.of_int i)

let of_int32 a = a
let to_int32 a = a

let less a b = Int32.sub a b < 0l
let less_equal a b = Int32.sub a b <= 0l
let greater a b = Int32.sub a b > 0l
let greater_equal a b = Int32.sub a b >= 0l
let equal a b = Int32.compare a b = 0

let min a b = if less a b then a else b
let max a b = if greater a b then a else b

let pp = Fmt.uint32
