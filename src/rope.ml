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
  | Str (s1, ofs1, len1), Str (s2, ofs2, len2) when len1 + len2 <= 0x7ff ->
      let str = String.concat "" [ String.sub s1 ofs1 len1; String.sub s2 ofs2 len2 ] in
      Str (str, 0, len1 + len2)
  | App (t1, Str (s1, ofs1, len1), _, _), Str (s2, ofs2, len2) when len1 + len2 <= 0x7ff ->
      let str = String.concat "" [ String.sub s1 ofs1 len1; String.sub s2 ofs2 len2 ] in
      App (t1, Str (str, 0, len1+len2), length t1 + len1 + len2, 1 + height t1)
  | Str (s1, ofs1, len1), App (Str (s2, ofs2, len2), t2, _, _) when len1 + len2 <= 0x7ff ->
      let str = String.concat "" [ String.sub s1 ofs1 len1; String.sub s2 ofs2 len2 ] in
      App (Str (str, 0, len1+len2), t2, len1 + len2 + length t2, 1 + height t2)
  | t1, t2 -> 
      App (t1, t2, length t1 + length t2, 1 + Int.max (height t1) (height t2))


let rec sub t start stop =
  if start == 0 && stop = length t
  then t else match t with
    | Str (str, off, _) ->
        Str (str, off + start, stop - start)
    | App (l, r, _, _) ->
        let len = length l in
        if stop <= len then sub l start stop
        else if start >= len then sub r (start - len) (stop - len)
        else append (sub l start len, sub r 0 (stop - len))

let sub t ~off ~len =
  let stop = off + len in
  if off < 0 || len < 0 || stop > length t
  then invalid_arg "Rope.sub";
  if len == 0 then t else sub t off stop

let shift t len = sub t ~off:len ~len:(length t - len)

let rec into_bytes buf dst_off = function
  | Str (str, src_off, len) -> Bytes.blit_string str src_off buf dst_off len
  | App (l, r, _, _) ->
    into_bytes buf dst_off l;
    into_bytes buf (dst_off + length l) r

let to_string t =
  let len = length t in
  let buf = Bytes.create len in
  into_bytes buf 0 t;
  Bytes.unsafe_to_string buf

let concat a b = append (a, b)
let prepend str t = append (Str (str, 0, String.length str), t)
let append t str = append (t, (Str (str, 0, String.length str)))
let of_string str = Str (str, 0, String.length str)

let equal a b = String.equal (to_string a) (to_string b)
