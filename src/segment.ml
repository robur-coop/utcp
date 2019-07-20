
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
  (* this is likely inefficient. the model uses explicit fields (and YY_discard
     bindings.

     not all combinations are of interest anyways:
     - PSH is rarely interesting (we will set whenever sndq is empty, when we
           receive, we should (explicitly) trigger a notify)
     - ACK is commonly in all segments (apart from the initial SYN!)
           research whether there are any interesting non-ack segments!?
     - FIN|SYN|RST are mutually exclusive (well, they are not, but I don't care
           about segments that have more than one of the three set

     when constructing, there's make_syn / make_syn_ack / make_rst explicit
     anyways - FIN goes through tcp_really_output with cansndmore on the
     connection

     -> we can ensure "only valid combinations" (by shifting + comparison), and
        then dress the type accordingly (flag : [`FIN | `SYN | `RST]), push
        being a bool, and ack being merged with the Sequence.t into an option!

     this way we get away from these cumbersome set / subset tests, drop bad
     segments early (net.inet.tcp.drop_synfin), and use proper types for the
     relevant flags. *)
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

(* auxFns:1520 *)
let make_rst_from_cb cb (_, src_port, dst, dst_port) =
  dst, { src_port ; dst_port ; seq = cb.State.snd_nxt ; ack = cb.rcv_nxt ;
         flags = Flags.(add `ACK (singleton `RST)) ;
         window = 0 ; options = [] ; payload = Cstruct.empty }

(* auxFns:2219 *)
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

(* auxFns:2331 *)
let drop_and_close id conn =
  match conn.State.tcp_state with
  | Syn_sent -> None
  | _ -> Some (make_rst_from_cb conn.control_block id)
 (* timed out and error handling (if err = timedout then cb.t_softerror ) *)

(* auxFns:1625 *)
let tcp_output_required now conn =
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
  (*: Calculate the amount of unused send window :*)
  let win = min cb.State.snd_wnd snd_cwnd in
  let snd_wnd_unused = win - (Sequence.window cb.State.snd_nxt cb.State.snd_una) in
  (*: Is it possible that a FIN may need to be sent? :*)
  let fin_required =
    conn.State.cantsndmore &&
    match conn.State.tcp_state with Fin_wait_2 | Time_wait -> false | _ -> true
  in
  (*: Under BSD, we may need to send a [[FIN]] in state [[SYN_SENT]] or [[SYN_RECEIVED]], so we may
      effectively still have a [[SYN]] on the send queue. :*)
  let syn_not_acked = match conn.State.tcp_state with Syn_sent | Syn_received -> true | _ -> false in
  (*: Is there data or a FIN to transmit? :*)
  let last_sndq_data_seq = Sequence.addi cb.State.snd_una (Cstruct.len conn.State.sndq) in
  let last_sndq_data_and_fin_seq =
    Sequence.(addi (addi last_sndq_data_seq (if fin_required then 1 else 0))
                (if syn_not_acked then 1 else 0))
  in
  let have_data_to_send = Sequence.less cb.snd_nxt last_sndq_data_seq in
  let have_data_or_fin_to_send = Sequence.less cb.snd_nxt last_sndq_data_and_fin_seq in
  (*: The amount by which the right edge of the advertised window could be moved :*)
  let window_update_delta =
    (min (Params.tcp_maxwin lsl cb.State.rcv_scale))
       (conn.rcvbufsize - Cstruct.len conn.rcvq) -
    Sequence.window cb.State.rcv_adv cb.State.rcv_nxt
  in
  (*: Send a window update? This occurs when (a) the advertised window can be increased by at
      least two maximum segment sizes, or (b) the advertised window can be increased by at least
      half the receive buffer size. See |tcp_output.c:322ff|. :*)
  let need_to_send_a_window_update =
    window_update_delta >= 2 * cb.State.t_maxseg || 2 * window_update_delta >= conn.rcvbufsize
  in
  (*: Note that silly window avoidance and [[max_sndwnd]] need to be dealt with
     here; see |tcp_output.c:309| :*)
  (*: Can a segment be transmitted? :*)
  let do_output =
    (*: Data to send and the send window has some space, or a FIN can be sent :*)
    (have_data_or_fin_to_send &&
     (have_data_to_send && snd_wnd_unused > 0)) || (* don't need space if only sending FIN *)
    (*: Can send a window update :*)
    need_to_send_a_window_update ||
    (*: An ACK should be sent immediately (e.g. in reply to a window probe) :*)
    cb.State.tf_shouldacknow
  in
  let persist_fun =
    let cant_send = not do_output && Cstruct.len conn.sndq = 0 && cb.State.tt_rexmt = None in
    let window_shrunk = win = 0 && snd_wnd_unused < 0 in  (*: [[win = 0]] if in [[SYN_SENT]], but still may send FIN :*)
                                                     (* (bsd_arch arch ==> tcp_sock.st <> SYN_SENT)) in *)
    if cant_send then  (* takes priority over window_shrunk; note this needs to be checked *)
      (*: Can not transmit a segment despite a non-empty send queue and no running persist or
          retransmit timer. Must be the case that the receiver's advertised window is now zero, so
          start the persist timer. Normal: |tcp_output.c:378ff| :*)
      Some (fun cb -> { cb with State.tt_rexmt = Subr.start_tt_persist now 0 cb.State.t_rttinf })
    else if window_shrunk then
        (*: The receiver's advertised window is zero and the receiver has retracted window space
            that it had previously advertised. Reset [[snd_nxt]] to [[snd_una]] because the data
            from [[snd_una]] to [[snd_nxt]] has likely not been buffered by the receiver and should
            be retransmitted. Bizzarely (on FreeBSD 4.6-RELEASE), if the persist timer is running
            reset its shift value :*)
        (* Window shrunk: |tcp_output.c:250ff| *)
      Some (fun cb ->
          let tt_rexmt = match cb.State.tt_rexmt with
            | Some ((Persist, _), d) -> Some ((State.Persist, 0), d)
            | _ -> Subr.start_tt_persist now 0 cb.t_rttinf
          in
          { cb with tt_rexmt ; snd_nxt = cb.snd_una })
    else
      (*: Otherwise, leave the persist timer alone :*)
      None
  in
  do_output, persist_fun

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
   let snd_wnd_unused = win - (Sequence.window cb.State.snd_nxt cb.State.snd_una) in
   let fin_required =
     conn.State.cantsndmore &&
     match conn.State.tcp_state with State.Fin_wait_2 | State.Time_wait -> false | _ -> true
   in
   let last_sndq_data_seq = Sequence.addi cb.State.snd_una (Cstruct.len conn.sndq) in
   (*: The data to send in this segment (if any) :*)
   let data' = Cstruct.shift conn.State.sndq (Sequence.window cb.State.snd_nxt cb.State.snd_una) in
   let data_to_send =
     Cstruct.sub data' 0 (min (Cstruct.len data') (min (max 0 snd_wnd_unused) cb.State.t_maxseg))
   in
   let dlen = Cstruct.len data_to_send in
   (*: Should [[FIN]] be set in this segment? :*)
   let fin = fin_required && Sequence.(greater_equal (addi cb.State.snd_nxt (Cstruct.len data_to_send)) last_sndq_data_seq) in
   (*: If this socket has previously sent a [[FIN]] which has not yet been acked, and [[snd_nxt]]
       is past the [[FIN]]'s sequence number, then [[snd_nxt]] should be set to the sequence number
       of the [[FIN]] flag, i.e. a retransmission. Check that [[snd_una <> iss]] as in this case no
       data has yet been sent over the socket  :*)
   let snd_nxt =
     if fin &&
        Sequence.equal (Sequence.addi cb.State.snd_nxt dlen) (Sequence.incr last_sndq_data_seq) &&
        not (Sequence.equal cb.State.snd_una cb.State.iss) ||
        Sequence.window cb.State.snd_nxt cb.State.iss = 2
     then
       Sequence.addi cb.State.snd_nxt (-1)
     else
       cb.State.snd_nxt
   in
   (*: The BSD way: set [[PSH]] whenever sending the last byte of data in the send queue :*)
   let psh = dlen > 0 && Sequence.equal (Sequence.addi cb.State.snd_nxt dlen) last_sndq_data_seq in
   (*: Calculate size of the receive window (based upon available buffer space) :*)
   let rcv_wnd' =
     let window_size = Sequence.window cb.State.rcv_adv cb.State.rcv_nxt in
     match conn.State.tcp_state with
     | Time_wait -> window_size
     | _ ->
       let rcv_wnd'' = Subr.calculate_bsd_rcv_wnd conn in
       max window_size
         (min (Params.tcp_maxwin lsl cb.State.rcv_scale)
            (if rcv_wnd'' < conn.rcvbufsize / 4 && rcv_wnd'' < cb.State.t_maxseg
             then 0  (*: Silly window avoidance: shouldn't advertise a tiny window :*)
             else rcv_wnd''))
    in
    (*: Advertise an appropriately scaled receive window :*)
    (*: Assert the advertised window is within a sensible range :*)
    let flags = Flags.of_list
        (`ACK :: (if psh then [ `PSH ] else []) @ (if fin then [ `FIN ] else []))
    in
    let seg =
      { src_port ; dst_port ; seq = snd_nxt;
        ack = cb.State.rcv_nxt; flags ; window = rcv_wnd' lsr cb.rcv_scale ;
        options = [] ; payload = data_to_send
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
    let snd_nxt' = Sequence.(addi (addi snd_nxt dlen) (if fin then 1 else 0)) in
    let snd_max = max cb.State.snd_max snd_nxt' in
    (*: Following a |tcp_output| code walkthrough by SB: :*)
    let tt_rexmt =
      if (State.mode_of cb.tt_rexmt = None ||
	  (State.mode_of cb.tt_rexmt = Some State.Persist && not window_probe)) &&
         Sequence.greater snd_nxt' cb.snd_una then
        (*: If the retransmit timer is not running, or the persist timer is
           running and this segment isn't a window probe, and this segment
           contains data or a [[FIN]] that occurs past [[snd_una]] (i.e.~new
           data), then start the retransmit timer. Note: if the persist timer is
           running it will be implicitly stopped :*)
        Subr.start_tt_rexmt now 0 false cb.t_rttinf
      else if window_probe && win0 <> 0 then
        (*: If the segment is a window probe, and in either case the send window
           is not closed, stop any running persist timer. :*)
        None (*: stop persisting :*)
      else
	(*: Otherwise, leave the timers alone :*)
        cb.tt_rexmt
    in
    (*: Time this segment if it is sensible to do so, i.e.~the following conditions hold : (a) a
        segment is not already being timed, and (b) data or a FIN are being sent, and (c) the
        segment being emitted is not a retransmit, and (d) the segment is not a window probe :*)
    let t_rttseg =
      if cb.State.t_rttseg = None &&
         (dlen > 0 || fin) &&
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

(* auxFns:2000 *)
let tcp_output_perhaps now id conn =
  let do_output, persist_fun = tcp_output_required now conn in
  let conn' = match persist_fun with
    | None -> conn
    | Some f ->
      let control_block = f conn.control_block in
      { conn with control_block }
  in
  if do_output then
    let conn'', seg = tcp_output_really now id false conn' in
    conn'', Some seg
  else
    conn', None

(* auxFns:1384 *)
let make_syn_ack cb (_, src_port, dst, dst_port) =
  let options =
    MaximumSegmentSize cb.State.t_advmss ::
    (match cb.request_r_scale with None -> [] | Some sc -> [ WindowScale sc ])
  in
  dst, { src_port ; dst_port ; seq = cb.iss ; ack = cb.rcv_nxt ;
         flags = Flags.of_list [ `SYN ; `ACK ] ;
         window = cb.rcv_wnd ; options ; payload = Cstruct.empty }

(* auxFns:1333 *)
let make_syn cb (_, src_port, dst, dst_port) =
  let options =
    MaximumSegmentSize cb.State.t_advmss ::
    (match cb.request_r_scale with None -> [] | Some sc -> [ WindowScale sc ])
  in
  dst, { src_port ; dst_port ; seq = cb.State.iss ; ack = Sequence.zero ;
         flags = Flags.singleton`SYN ;
         window = cb.rcv_wnd ; options ; payload = Cstruct.empty }

(* auxFns:1437 *)
let make_ack cb fin (_, src_port, dst, dst_port) =
  (* no need to clip, Segment does this for us *)
  let window = cb.State.rcv_wnd lsr cb.rcv_scale in
  (* sack *)
  dst, { src_port ; dst_port ;
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
