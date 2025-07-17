type t =
  | Str of Cstruct.t * int * int
  | App of t * t * int * int

let length = function
  | Str (_, _, len)
  | App (_, _, len, _) -> len

let empty = Str (Cstruct.empty, 0, 0)

let height = function
  | Str _ -> 0
  | App (_, _, _, h) -> h

let append = function
  | Str (_,_,0), t | t, Str (_,_,0) ->  t
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
  | Str (cs, src_off, len) -> Cstruct.blit_to_bytes cs src_off buf dst_off len
  | App (l, r, _, _) ->
    into_bytes buf dst_off l;
    into_bytes buf (dst_off + length l) r

let to_css t =
  let rec go acc = function
    | Str (_, _, 0) | Str ({ Cstruct.len= 0; _ }, _, _) -> acc
    | Str ({ Cstruct.len= rlen; _ } as cs, 0, len) ->
        if rlen == len then cs :: acc
        else Cstruct.sub cs 0 len :: acc
    | Str (cs, off, len) ->
        Cstruct.sub cs off len :: acc
    | App (l, r, _, _) -> go (go acc r) l in
  go [] t

let to_cs t =
  let buf = Cstruct.create (length t) in
  let rec go dst_off = function
    | Str (_, _, 0) | Str ({ Cstruct.len= 0; _ }, _, _) -> ()
    | Str (cs, src_off, len) -> Cstruct.blit cs src_off buf dst_off len
    | App (l, r, _, _) ->
        go dst_off l;
        go (dst_off + length l) r in
  go 0 t; buf

let to_string t =
  let len = length t in
  let buf = Bytes.create len in
  into_bytes buf 0 t;
  Bytes.unsafe_to_string buf

let concat a b = append (a, b)
let prepend ({ Cstruct.len; _ } as cs) t = append (Str (cs, 0, len), t)

let append t ?(off= 0) ?len ({ Cstruct.len= rlen; _ } as cs) =
  let len = match len with
    | Some len -> len | None -> rlen - off in
  append (t, (Str (cs, off, len)))

let of_cs ({ Cstruct.len; _ } as cs) = Str (cs, 0, len)
let of_css css = List.fold_left (fun t cs -> append t cs) empty css
let of_string str = of_cs (Cstruct.of_string str)

let equal a b =
  String.equal (to_string a) (to_string b)
