
let header_size = 20

let guard f e = if f then Ok () else Error e

(* looks like a good use case for gmap ;) *)
type option =
  | MaximumSegmentSize of int
  (* | WindowScale *)
  | Unknown of int * Cstruct.t

let pp_option ppf = function
  | MaximumSegmentSize mss -> Fmt.pf ppf "MSS %d" mss
  | Unknown (typ, v) -> Fmt.pf ppf "typ %X value %a" typ Cstruct.hexdump_pp v

let encode_option = function
  | MaximumSegmentSize mss ->
    let buf = Cstruct.create 4 in
    Cstruct.set_uint8 buf 0 2;
    Cstruct.set_uint8 buf 1 4;
    Cstruct.BE.set_uint16 buf 2 mss;
    buf
  | Unknown (typ, data) ->
    let buf = Cstruct.create 2 in
    Cstruct.set_uint8 buf 0 typ;
    Cstruct.set_uint8 buf 1 (Cstruct.len data);
    Cstruct.append buf data

let encode_options opts =
  let opts = Cstruct.concat (List.map encode_option opts) in
  let mod4len = Cstruct.len opts mod 4 in
  if mod4len > 0 then
    Cstruct.append opts (Cstruct.create (4 - mod4len))
  else
    opts

let decode_option data =
  let open Rresult.R.Infix in
  match Cstruct.get_uint8 data 0 with
  | 0 -> (* End of option, all remaining bytes must be 0 *)
    Ok (None, Cstruct.len data)
  | 1 -> (* No operation *) Ok (None, 1)
  | x ->
    guard (Cstruct.len data >= 2) (`Msg "option shorter than 2 bytes") >>= fun () ->
    let l = Cstruct.get_uint8 data 1 in
    guard (Cstruct.len data >= l) (`Msg "option too short") >>= fun () ->
    match x with
    | 2 ->
      guard (l = 4) (`Msg "length must be 4 in maximum segment size") >>= fun () ->
      let mss = Cstruct.BE.get_uint16 data 2 in
      Ok (Some (MaximumSegmentSize mss), 4)
    | _ ->
      Ok (Some (Unknown (x, Cstruct.sub data 2 (l - 2))), l)

let decode_options data =
  let open Rresult.R.Infix in
  let l = Cstruct.len data in
  let rec go idx acc =
    if l = idx then
      Ok acc
    else
      decode_option (Cstruct.shift data idx) >>= fun (data, consumed) ->
      let acc' = match data with None -> acc | Some x -> x :: acc in
      go (idx + consumed) acc'
  in
  go 0 []

module Flags = struct

  include Set.Make(struct
      type t = [ `FIN | `SYN | `RST | `PSH | `ACK ]
      let compare = compare
    end)

  let to_string = function
    | `FIN -> "F"
    | `SYN -> "S"
    | `RST -> "R"
    | `PSH -> "P"
    | `ACK -> "A"

  let pp ppf f = Fmt.(list ~sep:nop string) ppf (List.map to_string (elements f))

  let bit = function
    | `FIN -> 8
    | `SYN -> 7
    | `RST -> 6
    | `PSH -> 5
    | `ACK -> 4

  let number f = 1 lsl (8 - bit f)

  let all = [ `FIN ; `SYN ; `RST ; `PSH ; `ACK ]

  let decode byte =
    List.fold_left (fun flags flag ->
        if number flag land byte > 0 then add flag flags else flags)
    empty all

  let encode flags = fold (fun f acc -> acc + number f) flags 0

  let has ?(no = empty) ?(yes = empty) t =
    subset yes t && is_empty (inter no t)

  let only f t = singleton f = t

  let exact flags t = equal t (of_list flags)

  let or_ack f t =
    only f t || exact [ f ; `ACK ] t
end

type t = {
  src_port : int ;
  dst_port : int ;
  seq : Sequence.t ;
  ack : Sequence.t ;
  flags : Flags.t ;
  window : int ;
  options : option list ;
  payload : Cstruct.t ;
}

let mss t =
  List.fold_left
    (fun acc -> function MaximumSegmentSize x -> Some x | _ -> acc)
    None t.options

(* we always take our IP as source, thus of_segment -- to be used for a
   received segment -- needs to swap *)
let to_id ~src ~dst t = (dst, t.dst_port, src, t.src_port)

let pp ppf t =
  Fmt.pf ppf "%a@ seq %a@ ack %a@ window %d@ opts %a%d bytes data"
    Flags.pp t.flags Sequence.pp t.seq Sequence.pp t.ack
    t.window Fmt.(list ~sep:(unit ";@ ") pp_option) t.options
    (Cstruct.len t.payload)

let count_flags flags =
  (if Flags.mem `FIN flags then 1 else 0) + (if Flags.mem `SYN flags then 1 else 0)

let dropwithreset seg =
  if Flags.mem `RST seg.flags then
    None
  else
    let flags, ack, seq =
      if Flags.mem `ACK seg.flags then
        Flags.empty, Sequence.zero, seg.ack
      else
        let ack =
          let data_len = Cstruct.len seg.payload
          and flag_len = count_flags seg.flags
          in
          Sequence.(addi (addi seg.seq data_len) flag_len)
        in
        Flags.singleton `ACK, ack, Sequence.zero
    in
    Some { src_port = seg.dst_port ;
           dst_port = seg.src_port ;
           seq ; ack ;
           flags = Flags.add `RST flags ;
           window = 0 ; options = [] ; payload = Cstruct.empty }

(* auxFns:1774 no ts and arch, though *)
(* let tcp_output_really window_probe conn =
 *   let cb = conn.control_block in
 *   let snd_cwnd' =
 *     if  *)

let make_reset ?(options = []) cb ~src_port ~dst_port =
  { src_port ; dst_port ; seq = cb.State.snd_nxt (* or send_una + 1? *) ;
    ack = cb.State.rcv_nxt ; flags = Flags.of_list [ `RST ; `ACK ] ;
    window = cb.State.rcv_wnd ; options ; payload = Cstruct.empty }

let make_syn_ack ?(options = []) cb ~src_port ~dst_port =
  { src_port ; dst_port ; seq = cb.State.iss ; ack = cb.rcv_nxt ;
    flags = Flags.of_list [ `SYN ; `ACK ] ;
    window = cb.rcv_wnd ; options ; payload = Cstruct.empty }

let make_syn ?(options = []) cb ~src_port ~dst_port =
  { src_port ; dst_port ; seq = cb.State.iss ; ack = Sequence.zero ;
    flags = Flags.singleton`SYN ;
    window = cb.rcv_wnd ; options ; payload = Cstruct.empty }

let make_fin_ack ?(options = []) cb ~src_port ~dst_port =
  { src_port ; dst_port ; seq = cb.State.snd_nxt ; ack = cb.rcv_nxt ;
    flags = Flags.of_list [ `FIN ; `ACK ] ;
    window = cb.rcv_wnd ; options ; payload = Cstruct.empty }

let make_ack ?(options = []) cb ~src_port ~dst_port =
  { src_port ; dst_port ; seq = cb.State.snd_nxt ; ack = cb.rcv_nxt ;
    flags = Flags.singleton `ACK ;
    window = cb.rcv_wnd ; options ; payload = Cstruct.empty }

let checksum ~src ~dst buf =
  let plen = Cstruct.len buf in
  (* potentially pad *)
  let pad = plen mod 2 in
  (* construct pseudoheader *)
  let mybuf = Cstruct.create (12 + plen + pad) in
  Cstruct.BE.set_uint32 mybuf 0 (Ipaddr.V4.to_int32 src);
  Cstruct.BE.set_uint32 mybuf 4 (Ipaddr.V4.to_int32 dst);
  (* protocol is 0x0006 *)
  Cstruct.BE.set_uint16 mybuf 8 0x0006;
  Cstruct.BE.set_uint16 mybuf 10 plen;
  (* blit tcp packet *)
  Cstruct.blit buf 0 mybuf 12 plen;
  (* ensure checksum to be 0 *)
  Cstruct.BE.set_uint16 mybuf (12 + 16) 0;
  (* compute 2s complement 16 bit checksum *)
  let sum = ref 0 in
  (* compute checksum *)
  for i = 0 to pred (Cstruct.len mybuf / 2) do
    let v = Cstruct.BE.get_uint16 mybuf (i * 2) in
    let sum' = !sum + v in
    let sum'' = if sum' > 0xFFFF then succ sum' else sum' in
    sum := sum'' land 0xFFFF
  done ;
  (lnot !sum) land 0xFFFF

let encode t =
  let hdr = Cstruct.create header_size in
  let options = encode_options t.options in
  Cstruct.BE.set_uint16 hdr 0 t.src_port;
  Cstruct.BE.set_uint16 hdr 2 t.dst_port;
  Cstruct.BE.set_uint32 hdr 4 (Sequence.to_int32 t.seq);
  Cstruct.BE.set_uint32 hdr 8 (Sequence.to_int32 t.ack);
  Cstruct.set_uint8 hdr 12 ((header_size + Cstruct.len options) lsl 2); (* upper 4 bit, lower 4 reserved *)
  Cstruct.set_uint8 hdr 13 (Flags.encode t.flags);
  Cstruct.BE.set_uint16 hdr 14 t.window;
  Cstruct.concat [ hdr ; options ; t.payload ]

let encode_and_checksum ~src ~dst t =
  let data = encode t in
  let checksum = checksum ~src ~dst data in
  Cstruct.BE.set_uint16 data 16 checksum;
  data

let decode data =
  let open Rresult.R.Infix in
  guard (Cstruct.len data >= header_size) (`Msg "too small") >>= fun () ->
  let src_port = Cstruct.BE.get_uint16 data 0
  and dst_port = Cstruct.BE.get_uint16 data 2
  and seq = Sequence.of_int32 (Cstruct.BE.get_uint32 data 4)
  and ack = Sequence.of_int32 (Cstruct.BE.get_uint32 data 8)
  and data_off = (Cstruct.get_uint8 data 12 lsr 4) * 4 (* lower 4 are reserved [can't assume they're 0] *)
  and flags = Flags.decode (Cstruct.get_uint8 data 13)
  and window = Cstruct.BE.get_uint16 data 14
  and checksum = Cstruct.BE.get_uint16 data 16
  and _up = Cstruct.BE.get_uint16 data 18
  in
  guard (Cstruct.len data >= data_off) (`Msg "data_offset too big") >>= fun () ->
  let options_buf = Cstruct.sub data header_size (data_off - header_size) in
  decode_options options_buf >>| fun options ->
  let payload = Cstruct.shift data data_off in
  { src_port ; dst_port ; seq ; ack ; flags ; window ; options ; payload },
  checksum

let decode_and_validate ~src ~dst data =
  let open Rresult.R.Infix in
  decode data >>= fun (t, pkt_csum) ->
  let computed = checksum ~src ~dst data in
  (* these are already checks done in deliver_in_4, etc. *)
  guard (computed = pkt_csum) (`Msg "invalid checksum") >>= fun () ->
  guard Ipaddr.V4.(compare src broadcast <> 0)
    (`Msg "segment from broadcast") >>= fun () ->
  guard Ipaddr.V4.(compare dst broadcast <> 0)
    (`Msg "segment to broadcast") >>= fun () ->
  guard (not (Ipaddr.V4.is_multicast src))
    (`Msg "segment from multicast address") >>= fun () ->
  guard (not (Ipaddr.V4.is_multicast dst))
    (`Msg "segment to multicast address") >>= fun () ->
  guard (not ((Ipaddr.V4.compare src dst = 0 && t.src_port = t.dst_port)))
    (`Msg "segment source and destination ip and port are equal") >>| fun () ->
  t, to_id ~src ~dst t
