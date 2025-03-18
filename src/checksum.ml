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

let[@inline always] unsafe_feed_16_le ?(off = 0) ~len:top sum buf =
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
  !sum

(* NOTE(dinosaure): only work on 64-bit architecture. *)
let[@inline always] unsafe_feed_32_le ?(off = 0) ~len:top sum buf =
  let buf32 = to_int32 ~off ~len:top buf in
  let len = ref top in
  let sum = ref sum in
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
  !sum

(* NOTE(dinosaure): only work on 64-bit architecture. *)
let[@inline always] unsafe_feed_32_be ?(off = 0) ~len:top sum buf =
  let buf32 = to_int32 ~off ~len:top buf in
  let len = ref top in
  let sum = ref sum in
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
  !sum

let[@inline always] finally sum =
  let sum = ref sum in
  let car = ref (!sum lsr 16) in
  while !car != 0 do
    sum := (!sum land 0xffff) + !car;
    car := !sum lsr 16
  done;
  swap16 (lnot !sum land 0xffff)

let[@inline always] unsafe_feed ~off ~len sum buf =
  match Sys.word_size, Sys.big_endian with
  | 64, false -> unsafe_feed_32_le ~off ~len sum buf
  | 64, true -> unsafe_feed_32_be ~off ~len sum buf
  | 32, false -> unsafe_feed_16_le ~off ~len sum buf
  | n, be -> Fmt.invalid_arg "Unsupported platform (%d-bit, big-endian: %b)" n be

let unsafe_digest ~off ~len buf =
  let sum = (unsafe_feed[@inlined]) ~off ~len 0 buf in
  (finally[@inlined]) sum

let digest ?(off = 0) ?len buf =
  let len = match len with Some len -> len | None -> length buf - off in
  if len < 0
  || off < 0
  || off > length buf - len
  then invalid_arg "Cheksum.digest";
  unsafe_digest ~off ~len buf

let digest_cstruct { Cstruct.buffer; off; len } = unsafe_digest ~off ~len buffer
let feed_cstruct sum { Cstruct.buffer; off; len } = unsafe_feed ~off ~len sum buffer

external get_uint16_ne : string -> int -> int = "%caml_string_get16u"
external get_uint8 : string -> int -> int = "%string_unsafe_get"

let unsafe_feed_string_16_le ?(off = 0) ~len:top sum buf =
  let len = ref top in
  let sum = ref sum in
  let i = ref 0 in
  while !len >= 2 do
    sum := !sum + get_uint16_ne buf (!i * 2);
    incr i;
    len := !len - 2
  done;
  if !len = 1 then sum := !sum + get_uint8 buf (off + top - 1);
  !sum

let unsafe_feed_string_16_be ?(off = 0) ~len:top sum buf =
  let len = ref top in
  let sum = ref sum in
  let i = ref 0 in
  while !len >= 2 do
    sum := !sum + swap16 (get_uint16_ne buf (!i * 2));
    incr i;
    len := !len - 2
  done;
  if !len = 1 then sum := !sum + get_uint8 buf (off + top - 1);
  !sum

let feed_string ?(off= 0) ?len sum str =
  let len = match len with
    | Some len -> len
    | None -> String.length str - off in
  if off < 0
  || len < 0
  || off > String.length str - len
  then invalid_arg "Checksum.digest_string";
  if Sys.big_endian
  then unsafe_feed_string_16_be ~off ~len sum str
  else unsafe_feed_string_16_le ~off ~len sum str

let digest_string ?(off= 0) ?len str =
  let len = match len with
    | Some len -> len
    | None -> String.length str - off in
  if off < 0
  || len < 0
  || off > String.length str - len
  then invalid_arg "Checksum.digest_string";
  let sum =
    if Sys.big_endian
    then unsafe_feed_string_16_be ~off ~len 0 str
    else unsafe_feed_string_16_le ~off ~len 0 str in
  finally sum

let digest_strings sstr =
  let fn = match Sys.big_endian with
    | true -> fun sum str ->
        unsafe_feed_string_16_be ~off:0 ~len:(String.length str) sum str
    | false -> fun sum str ->
        unsafe_feed_string_16_le ~off:0 ~len:(String.length str) sum str in
  let sum = List.fold_left fn 0 sstr in
  finally sum
