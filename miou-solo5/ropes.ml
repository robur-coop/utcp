let src = Logs.Src.create "ropes"

module Log = (val Logs.src_log src : Logs.LOG)

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

let weight : type a. a t -> int = function
  | Str str -> String.length str
  | Unknown _ -> 0
  | App { weight; _ } -> weight

let ( <+> ) : type a. a size -> int -> a size = function
  | Length a -> fun b -> Length (a + b)
  | Limitless -> fun _ -> Limitless

let length : type a. a t -> a size = function
  | Unknown v ->  v
  | Str str -> Length (String.length str)
  | App { r_len= Limitless; _ } -> Limitless
  | App { l_len; r_len; _ } -> r_len <+> l_len

exception Out_of_bounds
exception Overlap

let () = Printexc.register_printer @@ function
  | Out_of_bounds -> Some "Fragment out of bounds"
  | Overlap -> Some "Fragment overlap"
  | _ -> None

let rec insert
  : type a. off:int -> string -> a t -> a t
  = fun ~off str -> function
  | Unknown Limitless ->
      let l = Unknown (Length off) in
      let rl = Str str in
      let rr = Unknown Limitless in
      let l_len = String.length str in
      let weight = String.length str in
      let r = App { l= rl; r= rr; weight; l_len; r_len= Limitless } in
      App { l; r; weight; l_len= off; r_len= Limitless }
  | Unknown (Length top) ->
      if off < 0
      || off > top - String.length str
      then raise_notrace Out_of_bounds;
      if off + String.length str == top
      then
        let l = Unknown (Length off) in
        let r = Str str in
        let len = String.length str in
        let weight = String.length str in
        App { l; r; weight; l_len= off; r_len= Length len }
      else
        let l = Unknown (Length off) in
        let rl = Str str in
        let r_len = Length (top - off - String.length str) in
        let rr = Unknown r_len in
        let len = String.length str in
        let weight = String.length str in
        let r = App { l= rl; r= rr; weight; l_len= len; r_len } in
        let r_len = r_len <+> String.length str in
        App { l; r; weight; l_len= off; r_len }
  | App { l; r; weight; l_len; r_len } ->
      if off < l_len
      then
        let l = insert ~off str l in
        let weight = weight + String.length str in
        App { l; r; weight; l_len; r_len }
      else
        let r = insert ~off:(off - l_len) str r in
        let weight = weight + String.length str in
        let r_len = length r in
        App { l; r; weight; l_len; r_len }
  | Str _ -> raise_notrace Overlap

let rec fixed : max:int -> unknown t -> fixed t
  = fun ~max -> function
  | Unknown Limitless -> Unknown (Length max)
  | App { l; r; weight; l_len; r_len= Limitless } ->
      let r = fixed ~max:(max - l_len) r in
      let r_len = length r in
      App { l; r; weight; l_len; r_len }

let to_bytes : fixed t -> Diet.t * bytes = fun t ->
  let Length len = length t in
  let buf = Bytes.create len in
  let rec go diet off = function
    | Str str ->
      let len = String.length str in
      Bytes.blit_string str 0 buf off len;
      Log.debug (fun m -> m "+[%d, %d]" off (off + len));
      Diet.add ~off ~len diet
    | Unknown (Length 0) -> diet
    | Unknown (Length len) ->
      Bytes.fill buf off len '\000';
      Log.debug (fun m -> m "+[%d, %d] (unknown)" off (off + len));
      Diet.add ~off ~len diet
    | App { l; r; l_len; _ } ->
      let diet = go diet off l in
      go diet (off + l_len) r in
  let diet = go Diet.empty 0 t in diet, buf
