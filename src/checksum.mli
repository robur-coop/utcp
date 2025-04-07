type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val feed_cstruct : int -> Cstruct.t -> int
val feed_string : off:int -> len:int -> int -> string -> int
val finally : int -> int

(**/*)

val unsafe_feed_16_le : off:int -> len:int -> int -> bigstring -> int
val unsafe_feed_32_le : off:int -> len:int -> int -> bigstring -> int
