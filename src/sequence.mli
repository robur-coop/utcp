(* (c) 2017 Hannes Mehnert, all rights reserved *)

type t = private int [@@immediate]

val of_int32 : int32 -> t
val to_int32 : t -> int32

val zero : t

val add : t -> t -> t
val incr : t -> t

val addi : t -> int -> t
external sub : t -> t -> int = "%subint"
external window : t -> t -> int = "%subint"

val less : t -> t -> bool
val less_equal : t -> t -> bool
val greater : t -> t -> bool
val greater_equal : t -> t -> bool
val equal : t -> t -> bool

val min : t -> t -> t
val max : t -> t -> t

val pp : t Fmt.t
