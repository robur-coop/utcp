type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let length x = Bigarray.Array1.dim x [@@inline]

let to_int32 ~off ~len :
    bigstring ->
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t =
 fun ba ->
  let pad = len mod 4 in
  let buf = Bigarray.Array1.sub ba off (len - pad) in
  Obj.magic buf

let to_int16 ~off ~len :
    bigstring ->
    (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
 fun ba ->
  let pad = len mod 2 in
  let buf = Bigarray.Array1.sub ba off (len - pad) in
  Obj.magic buf

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_unsafe_ref_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external int32_to_int : int32 -> int = "%int32_to_int"

let mask = 0xffff lsl 16 lor 0xffff
let int32_to_int n = int32_to_int n land mask

(* NOTE(dinosaure): Users may wonder why we use access with a bound-check (such
   as [bufX.{...}]). The reason is simple: OCaml, with regard to bigarrays, can
   unbox [int16]/[int32] if such access is desired. Furthermore, using a
   function such as [unsafe_get_int{16,32}] would not allow OCaml to correctly
   infer this access and to "prepare the ground" for unboxing. It should be
   noted that bound-check is not the most expensive (and quite predictable in
   reality), but unbox is. *)

let unsafe_digest_16_le ?(off = 0) ~len:top buf =
  let buf16 = to_int16 ~off ~len:top buf in
  let len = ref top in
  let sum = ref 0 in
  let i = ref 0 in
  while !len >= 2 do
    sum := !sum + buf16.{!i};
    if !sum > 0xffff then incr sum;
    sum := !sum land 0xffff;
    incr i;
    len := !len - 2
  done;
  if !len = 1 then sum := !sum + unsafe_get_uint8 buf (off + top - 1);
  if !sum > 0xffff then incr sum;
  swap16 (lnot !sum land 0xffff)

(* NOTE(dinosaure): only work on 64-bit architecture. *)
let unsafe_digest_32_le ?(off = 0) ~len:top buf =
  let buf32 = to_int32 ~off ~len:top buf in
  let len = ref top in
  let sum = ref 0 in
  let i = ref 0 in
  while !len >= 4 do
    let v = int32_to_int buf32.{!i} in
    sum := !sum + v;
    incr i;
    len := !len - 4
  done;
  if !len >= 2 then (
    sum := !sum + unsafe_get_uint16 buf (off + (!i * 4));
    len := !len - 2);
  if !len = 1 then sum := !sum + unsafe_get_uint8 buf (off + top - 1);
  while !sum lsr 16 <> 0 do
    sum := (!sum land 0xffff) + (!sum lsr 16)
  done;
  swap16 (lnot !sum land 0xffff)

(* NOTE(dinosaure): only work on 64-bit architecture. *)
let unsafe_digest_32_be ?(off = 0) ~len:top buf =
  let buf32 = to_int32 ~off ~len:top buf in
  let len = ref top in
  let sum = ref 0 in
  let i = ref 0 in
  while !len >= 4 do
    let v = int32_to_int (swap32 buf32.{!i}) in
    sum := !sum + v;
    incr i;
    len := !len - 4
  done;
  if !len >= 2 then (
    sum := !sum + swap16 (unsafe_get_uint16 buf (off + (!i * 4)));
    len := !len - 2);
  if !len = 1 then sum := !sum + unsafe_get_uint8 buf (off + top - 1);
  while !sum lsr 16 <> 0 do
    sum := (!sum land 0xffff) + (!sum lsr 16)
  done;
  swap16 (lnot !sum land 0xffff)

let digest ?(off = 0) ?len buf =
  let len = match len with Some len -> len | None -> length buf - off in
  match Sys.word_size, Sys.big_endian with
  | 32, false -> unsafe_digest_16_le ~off ~len buf
  | 64, false -> unsafe_digest_32_le ~off ~len buf
  | 64, true -> unsafe_digest_32_be ~off ~len buf
  | n, be ->
    invalid_arg
      ("don't know how to checksum on a " ^ string_of_int n ^ " bit " ^
       (if be then "big-endian" else "little-endian") ^ " machine")

let digest_cstruct { Cstruct.buffer; off; len } = digest ~off ~len buffer
