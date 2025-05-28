type fixed = |
type unknown = |

type 'a t =
  | Str : string -> fixed t
  | Unknown : 'a size -> 'a t
  | App : { l : fixed t; r : 'a t
          ; weight : int
          ; l_len : int
          ; r_len : 'a size } -> 'a t
and 'a size =
  | Length : int -> fixed size
  | Limitless : unknown size

exception Out_of_bounds
exception Overlap

val length : 'a t -> 'a size
val weight : 'a t -> int
val insert : off:int -> string -> 'a t -> 'a t
val fixed : max:int -> unknown t -> fixed t
val to_bytes : fixed t -> Diet.t * bytes
