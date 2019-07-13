
let header_size = 20

let guard f e = if f then Ok () else Error e

(* looks like a good use case for gmap ;) *)
type option =
  | MaximumSegmentSize of int
  | WindowScale of int
  | Unknown of int * Cstruct.t

let pp_option ppf = function
  | MaximumSegmentSize mss -> Fmt.pf ppf "MSS %d" mss
  | WindowScale f -> Fmt.pf ppf "window scale %d" f
  | Unknown (typ, v) -> Fmt.pf ppf "typ %X value %a" typ Cstruct.hexdump_pp v

let encode_option = function
  | MaximumSegmentSize mss ->
    let buf = Cstruct.create 4 in
    Cstruct.set_uint8 buf 0 2;
    Cstruct.set_uint8 buf 1 4;
    Cstruct.BE.set_uint16 buf 2 mss;
    buf
  | WindowScale f ->
    let buf = Cstruct.create 3 in
    Cstruct.set_uint8 buf 0 3;
    Cstruct.set_uint8 buf 1 3;
    Cstruct.set_uint8 buf 2 f;
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
  | 3 ->
    guard (Cstruct.len data >= 3) (`Msg "window scale shorter than 3 bytes") >>= fun () ->
    guard (Cstruct.get_uint8 data 1 = 3) (`Msg "window scale length not 3") >>| fun () ->
    Some (WindowScale (Cstruct.get_uint8 data 2)), 3
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

let ws t =
  List.fold_left
    (fun acc -> function WindowScale x -> Some x | _ -> acc)
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
let tcp_output_really now (_, src_port, dst, dst_port) window_probe conn =
   let cb = conn.State.control_block in
   let snd_cwnd =
     let rxtcur = Subr.computed_rxtcur cb.State.t_rttinf in
     let than = match Mtime.add_span cb.State.t_idletime (Mtime.Span.of_uint64_ns rxtcur) with
       | None -> assert false
       | Some ms -> ms
     in
     if Sequence.equal cb.State.snd_max cb.State.snd_una && Mtime.is_later ~than now then
       (*: The connection is idle and has been for >= 1RTO :*)
       (*: Reduce [[snd_cwnd]] to commence slow start :*)
       cb.State.t_maxseg * Params.ss_fltsz
     else
       cb.State.snd_cwnd
   in
   let win0 = min cb.State.snd_wnd snd_cwnd in
   let win = if window_probe && win0 = 0 then 1 else win0 in
   let _snd_wnd_unused = win - (Sequence.window cb.State.snd_nxt cb.State.snd_una) in
   let fin_required =
     conn.State.cantsndmore &&
     match conn.State.tcp_state with State.Fin_wait_2 | State.Time_wait -> false | _ -> true
   in
   let last_sndq_data_seq = cb.State.snd_una (* + LENGTH tcp_sock.sndq *) in
   (*: The data to send in this segment (if any) :*)
   (* let data' = DROP (Num (cb.snd_nxt - cb.snd_una)) tcp_sock.sndq in
    * let data_to_send = TAKE (MIN (clip_int_to_num snd_wnd_unused) cb.t_maxseg) data' in *)
   let data_to_send = Cstruct.empty in

   (*: Should [[FIN]] be set in this segment? :*)
   let fin = fin_required && Sequence.(greater_equal (addi cb.State.snd_nxt (Cstruct.len data_to_send)) last_sndq_data_seq) in

   (*: Should [[ACK]] be set in this segment? Under BSD, it is not set if the socket is in [[SYN_SENT]]
       and emitting a [[FIN]] segment due to [[shutdown()]] having been called. :*)
   (* let ack = if (bsd_arch arch /\ FIN /\ tcp_sock.st = SYN_SENT) then F else T in *)

   (*: If this socket has previously sent a [[FIN]] which has not yet been acked, and [[snd_nxt]]
       is past the [[FIN]]'s sequence number, then [[snd_nxt]] should be set to the sequence number
       of the [[FIN]] flag, i.e. a retransmission. Check that [[snd_una <> iss]] as in this case no
       data has yet been sent over the socket  :*)
   let snd_nxt =
     if fin &&
        Sequence.equal (Sequence.addi cb.State.snd_nxt (Cstruct.len data_to_send)) (Sequence.incr last_sndq_data_seq) &&
        not (Sequence.equal cb.State.snd_una cb.State.iss) ||
        Sequence.window cb.State.snd_nxt cb.State.iss = 2
     then
       Sequence.addi cb.State.snd_nxt (-1)
     else
       cb.State.snd_nxt
   in

   (*: The BSD way: set [[PSH]] whenever sending the last byte of data in the send queue :*)
   let psh = Cstruct.len data_to_send > 0 && Sequence.equal (Sequence.addi cb.State.snd_nxt (Cstruct.len data_to_send)) last_sndq_data_seq in

   (*: Calculate size of the receive window (based upon available buffer space) :*)
   let rcv_wnd'' = Subr.calculate_bsd_rcv_wnd conn in
   let rcv_wnd' =
     match conn.State.tcp_state with
     | Time_wait -> Sequence.window cb.State.rcv_adv cb.State.rcv_nxt
     | _ ->
       max (Sequence.window cb.State.rcv_adv cb.State.rcv_nxt)
         (min (Params.tcp_maxwin lsl cb.State.rcv_scale)
            (if rcv_wnd'' < conn.rcvbufsize / 4 && rcv_wnd'' < cb.State.t_maxseg
             then 0  (*: Silly window avoidance: shouldn't advertise a tiny window :*)
             else rcv_wnd''))
    in
    (*: Advertise an appropriately scaled receive window :*)
    (*: Assert the advertised window is within a sensible range :*)
    let window = min (rcv_wnd' lsr cb.State.rcv_scale) 65535 in
    let flags = Flags.of_list
        (`ACK :: (if psh then [ `PSH ] else []) @ (if fin then [ `FIN ] else []))
    in
    let seg =
      { src_port ; dst_port ; seq = snd_nxt;
        ack = cb.State.rcv_nxt; flags ; window ; options = [] ;
        payload = data_to_send
      }
    in

    (*: If emitting a [[FIN]] for the first time then change TCP state :*)
    let tcp_state =
      if fin then
        match conn.State.tcp_state with
        | Established -> State.Fin_wait_1
        | Close_wait -> State.Last_ack
        | x -> x
      else
        conn.State.tcp_state
    in

    (*: Updated values to store in the control block after the segment is output :*)
    let snd_nxt' = Sequence.(addi (addi snd_nxt (Cstruct.len data_to_send))
                               (if fin then 1 else 0))
    in
    let snd_max = max cb.State.snd_max snd_nxt' in

    (*: Following a |tcp_output| code walkthrough by SB: :*)
    let tt_rexmt = None in
(*      match cb.State.tt_rexmt with
      | None ->

      if (mode_of cb.tt_rexmt = NONE \/
		       (mode_of cb.tt_rexmt = SOME(Persist) /\ ~window_probe)) /\
                        snd_nxt'' > cb.snd_una then
                       (*: If the retransmit timer is not running, or the persist timer is running
                           and this segment isn't a window probe, and this segment contains data or
                           a [[FIN]] that occurs past [[snd_una]] (i.e.~new data), then start the
                           retransmit timer. Note: if the persist timer is running it will be
                           implicitly stopped :*)
                       start_tt_rexmt arch 0 F cb.t_rttinf
		    else if (window_probe \/ (IS_SOME tcp_sock.sndurp)) /\ win0 <> 0 /\
                       mode_of cb.tt_rexmt = SOME(Persist) then
                       (*: If the segment is a window probe or urgent data is being sent, and in
                           either case the send window is not closed, stop any running persist
                           timer. Note: if [[window_probe]] is [[T]] then a persist timer will
                           always be running but this isn't necessarily true when urgent data is
                           being sent :*)
                       NONE (*: stop persisting :*)
                    else
		       (*: Otherwise, leave the timers alone :*)
                       cb.tt_rexmt in *)

    (*: Time this segment if it is sensible to do so, i.e.~the following conditions hold : (a) a
        segment is not already being timed, and (b) data or a FIN are being sent, and (c) the
        segment being emitted is not a retransmit, and (d) the segment is not a window probe :*)
    let t_rttseg =
      if cb.State.t_rttseg = None &&
         (Cstruct.len data_to_send > 0 || fin) &&
         Sequence.greater snd_nxt' cb.State.snd_max &&
         not window_probe
      then
        Some (now, snd_nxt')
      else
        cb.State.t_rttseg
    in

    (*: Update the socket :*)
    let control_block = {
      cb with
      tt_rexmt ;
      snd_cwnd ;
      tf_rxwin0sent = (rcv_wnd' = 0) ;
      tf_shouldacknow = false ;
      t_rttseg ;
      snd_max ;
      snd_nxt = snd_nxt' ;
      tt_delack = None ;
      last_ack_sent = cb.State.rcv_nxt ;
      rcv_adv = Sequence.addi cb.State.rcv_nxt rcv_wnd'
    }
    in
    { conn with tcp_state ; control_block }, (dst, seg)

(* auxFns:1384 *)
let make_syn_ack cb (_, src_port, _, dst_port) =
  let window = min cb.State.rcv_wnd 65535 in
  let options =
    MaximumSegmentSize cb.t_advmss ::
    (if cb.tf_doing_ws then [ WindowScale cb.request_r_scale ] else [])
  in
  { src_port ; dst_port ; seq = cb.State.iss ; ack = cb.rcv_nxt ;
    flags = Flags.of_list [ `SYN ; `ACK ] ;
    window ; options ; payload = Cstruct.empty }

(* auxFns:1333 *)
let make_syn cb (_, src_port, _, dst_port) =
  let options =
    [ MaximumSegmentSize cb.State.t_advmss ;
      WindowScale cb.request_r_scale ]
  in
  { src_port ; dst_port ; seq = cb.State.iss ; ack = Sequence.zero ;
    flags = Flags.singleton`SYN ;
    window = cb.rcv_wnd ; options ; payload = Cstruct.empty }

(* auxFns:1437 *)
let make_ack cb fin (_, src_port, _, dst_port) =
  let window = min (cb.State.rcv_wnd lsr cb.rcv_scale) 65535 in
  (* sack *)
  { src_port ; dst_port ;
    seq = if fin then cb.snd_una else cb.snd_nxt ;
    ack = cb.rcv_nxt ;
    flags = Flags.add `ACK (if fin then Flags.singleton `FIN else Flags.empty) ;
    window ; options = [] ; payload = Cstruct.empty }

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
