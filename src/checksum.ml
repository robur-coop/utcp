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

external unsafe_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"
external unsafe_get_uint16 : bigstring -> int -> int = "%caml_bigstring_get16"
external swap16 : int -> int = "%bswap16"

let unsafe_digest_16 ~sum ~off ~len:top buf =
  let buf16 = to_int16 ~off ~len:top buf in
  let len = ref top in
  let sum = ref sum in
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
  lnot !sum land 0xffff

let unsafe_digest_32 ~sum ~off ~len:top buf =
  let buf32 = to_int32 ~off ~len:top buf in
  let len = ref top in
  let sum = ref sum in
  let i = ref 0 in
  while !len >= 4 do
    let[@warning "-8"] (Some v) = Int32.unsigned_to_int buf32.{!i} in
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
  lnot !sum land 0xffff

let digest ?sum ?(off = 0) ?len buf =
  let len = match len with Some len -> len | None -> length buf - off in
  let sum =
    let rev_sum s = lnot (if Sys.big_endian then s else swap16 s) land 0xffff in
    Option.value ~default:0 (Option.map rev_sum sum)
  in
  let csum =
    match Sys.word_size with
    | 32 -> unsafe_digest_16 ~sum ~off ~len buf
    | _ -> unsafe_digest_32 ~sum ~off ~len buf
  in
  if Sys.big_endian then csum else swap16 csum

let digest_cstruct ?sum { Cstruct.buffer; off; len } =
  digest ?sum ~off ~len buffer
