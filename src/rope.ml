type t =
  | Str of string * int * int
  | App of t * t * int * int

let length = function
  | Str (_, _, len)
  | App (_, _, len, _) -> len

let empty = Str (String.empty, 0, 0)

let height = function
  | Str _ -> 0
  | App (_, _, _, h) -> h

let append = function
  | Str (_,_,0), t | t, Str (_,_,0) ->  t
  (* NOTE(dinosaure): we can coalesce strings and be sure that the allocation
     still is localized into the minor heap or we can just keep chunks as they
     are. We already to the fragmentation to small pieces of strings when we
     pass from a [Cstruct.t] to some strings.

  | Str (s1, ofs1, len1), Str (s2, ofs2, len2) when len1 + len2 <= 0x7ff ->
      let buf = Bytes.create (len1 + len2) in
      Bytes.blit_string s1 ofs1 buf 0 len1;
      Bytes.blit_string s2 ofs2 buf len1 len2;
      Str (Bytes.unsafe_to_string buf, 0, len1 + len2)
  | App (t1, Str (s1, ofs1, len1), _, _), Str (s2, ofs2, len2) when len1 + len2 <= 0x7ff ->
      let buf = Bytes.create (len1 + len2) in
      Bytes.blit_string s1 ofs1 buf 0 len1;
      Bytes.blit_string s2 ofs2 buf len1 len2;
      App (t1, Str (Bytes.unsafe_to_string buf, 0, len1 + len2), length t1 + len1 + len2, 1 + height t1)
  | Str (s1, ofs1, len1), App (Str (s2, ofs2, len2), t2, _, _) when len1 + len2 <= 0x7ff ->
      let buf = Bytes.create (len1 + len2) in
      Bytes.blit_string s1 ofs1 buf 0 len1;
      Bytes.blit_string s2 ofs2 buf len1 len2;
      App (Str (Bytes.unsafe_to_string buf, 0, len1 + len2), t2, len1 + len2 + length t2, 1 + height t2)
  *)
  | t1, t2 ->
      App (t1, t2, length t1 + length t2, 1 + Int.max (height t1) (height t2))

let rec unsafe_sub t start stop =
  if start == 0 && stop = length t
  then t else match t with
    | Str (str, off, _) ->
        Str (str, off + start, stop - start)
    | App (l, r, _, _) ->
        let len = length l in
        if stop <= len then unsafe_sub l start stop
        else if start >= len then unsafe_sub r (start - len) (stop - len)
        else append (unsafe_sub l start len, unsafe_sub r 0 (stop - len))

let chop t len =
  if len < 0 || len > length t
  then invalid_arg "Rope.chop";
  if len == 0 then empty else unsafe_sub t 0 len

let shift t len =
  if len < 0 then t
  else if len == 0 then t
  else
    let max = length t in
    let len = Int.min max len in
    unsafe_sub t len (len + (max - len))

let rec into_bytes buf dst_off = function
  | Str (str, src_off, len) -> Bytes.blit_string str src_off buf dst_off len
  | App (l, r, _, _) ->
    into_bytes buf dst_off l;
    into_bytes buf (dst_off + length l) r

let to_strings t =
  let rec go acc = function
    | Str (_, _, 0) -> acc
    | Str (str, 0, len) when String.length str == len -> str :: acc
    | Str (str, off, len) -> String.sub str off len :: acc
    | App (l, r, _, _) -> go (go acc r) l in
  go [] t

let to_string t =
  let len = length t in
  let buf = Bytes.create len in
  into_bytes buf 0 t;
  Bytes.unsafe_to_string buf

let concat a b = append (a, b)
let prepend str t = append (Str (str, 0, String.length str), t)

let append t ?(off = 0) ?len str =
  let len = match len with
    | Some len -> len
    | None -> String.length str - off
  in
  append (t, (Str (str, off, len)))

let of_string str = Str (str, 0, String.length str)
let of_strings strs = List.fold_left (fun t str -> append t str) empty strs
let equal a b = String.equal (to_string a) (to_string b)
