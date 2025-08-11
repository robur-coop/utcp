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

val to_css : t -> Cstruct.t list
(** [to_css t] compiles the given rope [t] into a series of [Cstruct.t].
    Invariant: these [Cstruct.t] are never empty. [to_css] does not allocate
    bytes. *)

val to_cs : t -> Cstruct.t
(** [to_cs] compiles the given rope [t] into an allocated [Cstruct.t]. *)

val to_string : t -> string
(** [to_string] compiles the given rope [t]) into an allocated [string]. *)

val concat : t -> t -> t
(** [concat t0 t1] concatenates the rope [t0] and [t1]. *)

val prepend : Cstruct.t -> t -> t
(** [prepend cs t] is [concat (of_cs cs) t]. *)

val append : t -> ?off:int -> ?len:int -> Cstruct.t -> t
(** [append t ?off ?len cs] is [concat t (of_cs (Cstruct.sub ?off ?len cs))]. *)

val of_cs : Cstruct.t -> t
(** [of_cs cs] is a rope from a [Cstruct.t] [cs]. *)

val of_css : Cstruct.t list -> t
(** [of_css css] is a rope from a list of [Cstruct.t] [css] (empty [Cstruct.t]
    are not take into account). *)

val of_string : string -> t
(** [of_string str] is a rope from a [string] [str]. *)

val equal : t -> t -> bool
