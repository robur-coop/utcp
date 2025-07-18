type t (** Immutable rope *)

val length : t -> int
(** [length t]returns the length of the rope (O(1) time). *)

val empty : t
(** An empty rope. *)

val chop : t -> int -> t
(** [chop len t] returns a new rope that retains [len] bytes of the given rope
    [t]. *)

val shift : t -> int -> t
(** [shift t len] produces a new rope offset by [len] bytes from the given rope
    [t]. *)

val to_string : t -> string
(** [to_string] compiles the given rope [t]) into an allocated [string]. *)

val to_strings : t -> string list

val concat : t -> t -> t
(** [concat t0 t1] concatenates the rope [t0] and [t1]. *)

val prepend : string -> t -> t
(** [prepend cs t] is [concat (of_cs cs) t]. *)

val append : t -> ?off:int -> ?len:int -> string -> t
(** [append t ?off ?len cs] is [concat t (of_cs (String.sub ?off ?len cs))]. *)

val of_string : string -> t
(** [of_string str] is a rope from a [string] [str]. *)

val of_strings : string list -> t

val equal : t -> t -> bool
