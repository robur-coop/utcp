type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let length x = Bigarray.Array1.dim x [@@inline]

let to_int32 :
    bigstring ->
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t =
 fun ba ->
  let len = length ba in
  let pad = len mod 4 in
  let buf = Bigarray.Array1.sub ba 0 (len - pad) in
  Obj.magic buf

let to_int16 :
    bigstring ->
    (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
 fun ba ->
  let len = length ba in
  let pad = len mod 2 in
  let buf = Bigarray.Array1.sub ba 0 (len - pad) in
  Obj.magic buf

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external swap16 : int -> int = "%bswap16"

let unsafe_digest_16 ?(off = 0) ~len:top buf =
  let buf16 = to_int16 buf in
  let len = ref top in
  let sum = ref 0 in
  let i = ref 0 in
  while !len >= 2 do
    sum := !sum + buf16.{off + !i};
    if !sum > 0xffff then incr sum;
    sum := !sum land 0xffff;
    incr i;
    len := !len - 2
  done;
  if !len = 1 then sum := !sum + unsafe_get_uint8 buf (off + top - 1);
  if !sum > 0xffff then incr sum;
  swap16 (lnot !sum land 0xffff)

let unsafe_digest_32 ?(off = 0) ~len:top buf =
  let buf32 = to_int32 buf in
  let len = ref top in
  let sum = ref 0 in
  let i = ref 0 in
  while !len >= 4 do
    let[@warning "-8"] (Some v) = Int32.unsigned_to_int buf32.{off + !i} in
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

let digest ?(off = 0) ?len buf =
  let len = match len with Some len -> len | None -> length buf - off in
  match Sys.word_size with
  | 32 -> unsafe_digest_16 ~off ~len buf
  | _ -> unsafe_digest_32 ~off ~len buf

let digest_cstruct { Cstruct.buffer; off; len } = digest ~off ~len buffer
