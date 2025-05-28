type t

val empty : t
val add : off:int -> len:int -> t -> t
val diff : t -> t -> t
val is_empty : t -> bool
