type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val unsafe_digest_16_le : ?off:int -> len:int -> bigstring -> int
val unsafe_digest_32_le : ?off:int -> len:int -> bigstring -> int
val unsafe_digest_32_be : ?off:int -> len:int -> bigstring -> int
val digest : ?off:int -> ?len:int -> bigstring -> int
val digest_cstruct : Cstruct.t -> int
