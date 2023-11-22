type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val unsafe_digest_16 : sum:int -> off:int -> len:int -> bigstring -> int
val unsafe_digest_32 : sum:int -> off:int -> len:int -> bigstring -> int
val digest : ?sum:int -> ?off:int -> ?len:int -> bigstring -> int
val digest_cstruct : ?sum:int -> Cstruct.t -> int
