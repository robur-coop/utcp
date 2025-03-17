
let src = Logs.Src.create "tcp.segment" ~doc:"TCP segment"
module Log = (val Logs.src_log src : Logs.LOG)

let header_size = 20

let guard f e = if f then Ok () else Error e

let ( let* ) = Result.bind

(* looks like a good use case for gmap ;) *)
type tcp_option =
  | MaximumSegmentSize of int
  | WindowScale of int
  | Unknown of int * Cstruct.t

let equal_option a b = match a, b with
  | MaximumSegmentSize a, MaximumSegmentSize b -> a = b
  | WindowScale a, WindowScale b -> a = b
  | Unknown (t, v), Unknown (t', v') -> t = t' && Cstruct.equal v v'
  | _ -> false

let pp_option ppf = function
  | MaximumSegmentSize mss -> Fmt.pf ppf "MSS %d" mss
  | WindowScale f -> Fmt.pf ppf "window scale %d" f
  | Unknown (typ, v) -> Fmt.pf ppf "typ %X value %a" typ Cstruct.hexdump_pp v

let encode_option buf off = function
  | MaximumSegmentSize mss ->
    Cstruct.set_uint8 buf off 2;
    Cstruct.set_uint8 buf (off + 1) 4;
    Cstruct.BE.set_uint16 buf (off + 2) mss;
    off + 4
  | WindowScale f ->
    Cstruct.set_uint8 buf off 3;
    Cstruct.set_uint8 buf (off + 1) 3;
    Cstruct.set_uint8 buf (off + 2) f;
    off + 3
  | Unknown (typ, data) ->
    let len = Cstruct.length data in
    Cstruct.set_uint8 buf off typ;
    Cstruct.set_uint8 buf (off + 1) len;
    Cstruct.blit data 0 buf (off + 2) len;
    off + len + 2

let length_options opts =
  let needed =
    List.fold_left (fun l -> function
        | MaximumSegmentSize _ -> 4 + l
        | WindowScale _ -> 3 + l
        | Unknown (_, data) -> Cstruct.length data + 2 + l)
      0 opts
  in
  ((needed + 3) lsr 2) lsl 2

let encode_options buf off opts =
  List.fold_left (encode_option buf) off opts

let decode_option data =
  match Cstruct.get_uint8 data 0 with
  | 0 -> (* End of option, all remaining bytes must be 0 *)
    Ok (None, Cstruct.length data)
  | 1 -> (* No operation *) Ok (None, 1)
  | 3 ->
    let* () =
      guard (Cstruct.length data >= 3) (`Msg "window scale shorter than 3 bytes")
    in
    let* () =
      guard (Cstruct.get_uint8 data 1 = 3) (`Msg "window scale length not 3")
    in
    Ok (Some (WindowScale (Cstruct.get_uint8 data 2)), 3)
  | x ->
    let* () =
      guard (Cstruct.length data >= 2) (`Msg "option shorter than 2 bytes")
    in
    let l = Cstruct.get_uint8 data 1 in
    let* () = guard (Cstruct.length data >= l) (`Msg "option too short") in
    match x with
    | 2 ->
      let* () =
        guard (l = 4) (`Msg "maximum segment size must be at least 4 bytes")
      in
      let mss = Cstruct.BE.get_uint16 data 2 in
      Ok (Some (MaximumSegmentSize mss), 4)
    | _ ->
      Ok (Some (Unknown (x, Cstruct.sub data 2 (l - 2))), l)

let decode_options data =
  let l = Cstruct.length data in
  let rec go idx acc =
    if l = idx then
      Ok acc
    else
      let* data, consumed = decode_option (Cstruct.shift data idx) in
      let acc' = match data with None -> acc | Some x -> x :: acc in
      go (idx + consumed) acc'
  in
  go 0 []

module Flag = struct
  type flags =
    | FIN
    | SYN
    | RST
    | PSH
    | ACK

  let number =
    let fin = 1 lsl 0
    and syn = 1 lsl 1
    and rst = 1 lsl 2
    and psh = 1 lsl 3
    and ack = 1 lsl 4
    in
    function
    | FIN -> fin
    | SYN -> syn
    | RST -> rst
    | PSH -> psh
    | ACK -> ack

  let decode byte =
    let ack = number ACK land byte > 0
    and psh = number PSH land byte > 0
    and fin = number FIN land byte > 0
    and syn = number SYN land byte > 0
    and rst = number RST land byte > 0
    in
    let* flag =
      match fin, syn, rst with
      | true, false, false -> Ok (Some `Fin)
      | false, true, false -> Ok (Some `Syn)
      | false, false, true -> Ok (Some `Rst)
      | false, false, false -> Ok None
      | _ -> Error (`Msg (Fmt.str "invalid flag combination: %02X" byte))
    in
    Ok (ack, flag, psh)

  let encode (ack, flag, psh) =
    (if ack then number ACK else 0) +
    (if psh then number PSH else 0) +
    (match flag with
     | None -> 0
     | Some `Fin -> number FIN
     | Some `Syn -> number SYN
     | Some `Rst -> number RST)

  let eq a b = match a, b with
    | None, None -> true
    | Some a, Some b ->
      begin match a, b with
        | `Syn, `Syn | `Fin, `Fin | `Rst, `Rst -> true
        | _ -> false
      end
    | _ -> false

  let pp ppf = function
    | None -> Fmt.string ppf ""
    | Some `Syn -> Fmt.string ppf "S"
    | Some `Fin -> Fmt.string ppf "F"
    | Some `Rst -> Fmt.string ppf "R"
end

type t = {
  src_port : int ;
  dst_port : int ;
  seq : Sequence.t ;
  ack : Sequence.t option ;
  flag : [ `Syn | `Fin | `Rst ] option ;
  push : bool ;
  window : int ;
  options : tcp_option list ;
  payload : Cstruct.t ;
}

let equal a b =
  a.src_port = b.src_port && a.dst_port = b.dst_port &&
  Sequence.equal a.seq b.seq &&
  (match a.ack, b.ack with
   | None, None -> true | Some a, Some b -> Sequence.equal a b | _ -> false) &&
  Flag.eq a.flag b.flag &&
  a.push = b.push &&
  a.window = b.window &&
  List.length a.options = List.length b.options &&
  List.for_all2 equal_option a.options b.options &&
  Cstruct.equal a.payload b.payload

let max_win = 1 lsl 16 - 1

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
  Fmt.pf ppf "%a%s@ seq %a@ ack %a@ window %d@ opts %a, %d bytes data"
    Flag.pp t.flag (if t.push then "P" else "")
    Sequence.pp t.seq
    Fmt.(option ~none:(any "no") Sequence.pp) t.ack
    t.window Fmt.(list ~sep:(any ";@ ") pp_option) t.options
    (Cstruct.length t.payload)

let count_flags = function
  | Some (`Fin | `Syn) -> 1
  | _ -> 0

(* auxFns:1520 *)
let make_rst_from_cb cb (src, src_port, dst, dst_port) =
  src, dst,
  { src_port ; dst_port ; seq = cb.State.snd_nxt ; ack = Some cb.rcv_nxt ;
    flag = Some `Rst ; push = false ; window = 0 ; options = [] ;
    payload = Cstruct.empty }

(* auxFns:2219 *)
let dropwithreset seg =
  match seg.flag with
  | Some `Rst -> None
  | _ ->
    let ack, seq =
      match seg.ack with
      | Some ack -> None, ack (* never ACK an ACK *)
      | None ->
        let ack =
          let data_len = Cstruct.length seg.payload
          and flag_len = count_flags seg.flag
          in
          Sequence.(addi (addi seg.seq data_len) flag_len)
        in
        Some ack, Sequence.zero
    in
    Some { src_port = seg.dst_port ;
           dst_port = seg.src_port ;
           seq ; ack ;
           flag = Some `Rst ; push = false ;
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
  let last_sndq_data_seq = Sequence.addi cb.State.snd_una (Cstruct.lenv conn.State.sndq) in
  let last_sndq_data_and_fin_seq =
    Sequence.(addi (addi last_sndq_data_seq (if fin_required then 1 else 0))
                (if syn_not_acked then 1 else 0))
  in
  let have_data_to_send = Sequence.less cb.snd_nxt last_sndq_data_seq in
  let have_data_or_fin_to_send = Sequence.less cb.snd_nxt last_sndq_data_and_fin_seq in
  (*: The amount by which the right edge of the advertised window could be moved :*)
  let window_update_delta =
    (min (Params.tcp_maxwin lsl cb.State.rcv_scale))
       (conn.rcvbufsize - Cstruct.lenv conn.rcvq) -
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
     (if have_data_to_send then snd_wnd_unused > 0 else true)) || (* don't need space if only sending FIN *)
    (*: Can send a window update :*)
    need_to_send_a_window_update ||
    (*: An ACK should be sent immediately (e.g. in reply to a window probe) :*)
    cb.State.tf_shouldacknow
  in
  let persist_fun =
    let cant_send = not do_output && Cstruct.lenv conn.sndq = 0 && cb.State.tt_rexmt = None in
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
let tcp_output_really_helper now (src, src_port, dst, dst_port) window_probe conn =
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
  let last_sndq_data_seq = Sequence.addi cb.State.snd_una (Cstruct.lenv conn.sndq) in
  (*: The data to send in this segment (if any) :*)
  let data_to_send, more_data_could_be_sent =
    let data' =
      Cstruct.shiftv (List.rev conn.State.sndq)
        (max 0
           (min (Cstruct.lenv conn.State.sndq)
              (Sequence.window cb.State.snd_nxt cb.State.snd_una)))
        (* taking the minimum to avoid exceeding the sndq *)
    in
    let data' = Cstruct.concat data' in
    let len_could_be_sent = max 0 snd_wnd_unused in
    let dlen = Cstruct.length data' in
    Cstruct.sub data' 0 (min dlen (min len_could_be_sent cb.State.t_maxseg)),
    dlen > cb.t_maxseg && len_could_be_sent > cb.t_maxseg
  in
  let dlen = Cstruct.length data_to_send in
  (*: Should [[FIN]] be set in this segment? :*)
  let fin = fin_required && Sequence.(greater_equal (addi cb.State.snd_nxt dlen) last_sndq_data_seq) in
  (*: If this socket has previously sent a [[FIN]] which has not yet been acked, and [[snd_nxt]]
      is past the [[FIN]]'s sequence number, then [[snd_nxt]] should be set to the sequence number
      of the [[FIN]] flag, i.e. a retransmission. Check that [[snd_una <> iss]] as in this case no
      data has yet been sent over the socket  :*)
  let snd_nxt =
    if fin &&
       (Sequence.equal (Sequence.addi cb.State.snd_nxt dlen) (Sequence.incr last_sndq_data_seq) &&
        not (Sequence.equal cb.State.snd_una cb.State.iss) ||
        Sequence.window cb.State.snd_nxt cb.State.iss = 2)
    then
      Sequence.addi cb.State.snd_nxt (-1)
    else
      cb.State.snd_nxt
  in
  (*: The BSD way: set [[PSH]] whenever sending the last byte of data in the send queue :*)
  let push = dlen > 0 && Sequence.equal (Sequence.addi cb.State.snd_nxt dlen) last_sndq_data_seq in
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
  let flag = if fin then Some `Fin else None in
  let seg =
    { src_port ; dst_port ; seq = snd_nxt;
      ack = Some cb.State.rcv_nxt ; flag ; push ;
      window = min (rcv_wnd' lsr cb.rcv_scale) max_win ;
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
    else if window_probe && win0 <> 0 && State.mode_of cb.tt_rexmt = Some State.Persist then
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
    rcv_adv = Sequence.addi cb.State.rcv_nxt rcv_wnd' ;
    rcv_wnd = rcv_wnd' ;
    (* the rcv_wnd update does not occur in the model, the reasoning is
       TCP1_hostTypesScript.sml:538: "Don't check equality of [[rcv_wnd]]: we
       recalculate [[rcv_wnd]] lazily in [[tcp_output]] instead of after every
       successful [[recv()]] call, so our value is often out of date."
       we're doing this update here, since we use cb rcv_wnd in the in_window
       check in input.ml *)
  } in
  { conn with tcp_state ; control_block }, (src, dst, seg), more_data_could_be_sent

let tcp_output_really now (src, src_port, dst, dst_port) window_probe conn =
  let cb, seg, _ = tcp_output_really_helper now (src, src_port, dst, dst_port) window_probe conn in
  cb, seg

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
    let rec send_more conn acc =
      let conn', seg, more_possible = tcp_output_really_helper now id false conn in
      let outs = seg :: acc in
      if more_possible then
        send_more conn' outs
      else
        conn', outs
    in
    let conn', outs = send_more conn' [] in
    conn', List.rev outs
  else
    conn', []

(* auxFns:1384 *)
let make_syn_ack cb (src, src_port, dst, dst_port) =
  let window = min cb.State.rcv_wnd max_win in
  let options =
    MaximumSegmentSize cb.t_advmss ::
    (Option.map (fun sc -> WindowScale sc) cb.request_r_scale |> Option.to_list)
  in
  src, dst,
  { src_port ; dst_port ; seq = cb.iss ; ack = Some cb.rcv_nxt ;
    flag = Some `Syn ; push = false ; window ; options ;
    payload = Cstruct.empty }

(* auxFns:1333 *)
let make_syn cb (src, src_port, dst, dst_port) =
  let window = min cb.State.rcv_wnd max_win in
  let options =
    MaximumSegmentSize cb.State.t_advmss ::
    (Option.map (fun sc -> WindowScale sc) cb.request_r_scale |> Option.to_list)
  in
  src, dst,
  { src_port ; dst_port ; seq = cb.State.iss ; ack = None ;
    flag = Some `Syn ; push = false ;
    window ; options ; payload = Cstruct.empty }

(* auxFns:1437 *)
let make_ack cb ~fin (src, src_port, dst, dst_port) =
  let window = min (cb.State.rcv_wnd lsr cb.rcv_scale) max_win in
  (* sack *)
  src, dst,
  { src_port ; dst_port ;
    seq = if fin then cb.snd_una else cb.snd_nxt ;
    ack = Some cb.rcv_nxt ;
    flag = if fin then Some `Fin else None ;
    push = false ; window ; options = [] ; payload = Cstruct.empty }

let checksum ~src ~dst buf =
  let plen = Cstruct.length buf in
  (* construct pseudoheader *)
  let mybuf, off =
    let protocol = 0x06 in
    match src, dst with
    | Ipaddr.V4 src, Ipaddr.V4 dst ->
      let mybuf = Cstruct.create (12 + plen) in
      Ipaddr_cstruct.V4.write_cstruct_exn src mybuf;
      Ipaddr_cstruct.V4.write_cstruct_exn dst (Cstruct.shift mybuf 4);
      Cstruct.set_uint8 mybuf 9 protocol;
      Cstruct.BE.set_uint16 mybuf 10 plen;
      mybuf, 12
    | Ipaddr.V6 src, Ipaddr.V6 dst ->
      let mybuf = Cstruct.create (40 + plen) in
      Ipaddr_cstruct.V6.write_cstruct_exn src mybuf;
      Ipaddr_cstruct.V6.write_cstruct_exn dst (Cstruct.shift mybuf 16);
      Cstruct.BE.set_uint16 mybuf 34 plen;
      Cstruct.set_uint8 mybuf 39 protocol;
      mybuf, 40
    | _ -> invalid_arg "mixing IPv4 and IPv6 addresses not supported"
  in
  (* blit tcp packet *)
  Cstruct.blit buf 0 mybuf off plen;
  (* ensure checksum to be 0 *)
  Cstruct.BE.set_uint16 mybuf (off + 16) 0;
  (* compute checksum *)
  Checksum.digest_cstruct mybuf

let encode_into buf t =
  let opt_len = length_options t.options in
  Cstruct.BE.set_uint16 buf 0 t.src_port;
  Cstruct.BE.set_uint16 buf 2 t.dst_port;
  Cstruct.BE.set_uint32 buf 4 (Sequence.to_int32 t.seq);
  Cstruct.BE.set_uint32 buf 8 (match t.ack with None -> 0l | Some a -> Sequence.to_int32 a);
  Cstruct.set_uint8 buf 12 ((header_size + opt_len) lsl 2); (* upper 4 bit, lower 4 reserved *)
  Cstruct.set_uint8 buf 13 (Flag.encode ((match t.ack with None -> false | Some _ -> true), t.flag, t.push));
  Cstruct.BE.set_uint16 buf 14 t.window;
  let _ = encode_options buf 20 t.options in
  Cstruct.blit t.payload 0 buf (20 + opt_len) (Cstruct.length t.payload)

let length t =
  header_size + Cstruct.length t.payload + length_options t.options

let encode t =
  let buf = Cstruct.create (length t) in
  encode_into buf t;
  buf

let encode_and_checksum_into now buf ~src ~dst t =
  encode_into buf t;
  let checksum = checksum ~src ~dst buf in
  Cstruct.BE.set_uint16 buf 16 checksum;
  State.Tracing.debug (fun m -> m "%a [%a] out %u %s"
                          State.Connection.pp (src, t.src_port, dst, t.dst_port)
                          Mtime.pp now (Cstruct.length t.payload)
                          (Base64.encode_string (Cstruct.to_string buf)))

let encode_and_checksum now ~src ~dst t =
  let buf = Cstruct.create (length t) in
  encode_and_checksum_into now buf ~src ~dst t;
  buf

let decode data =
  let* () = guard (Cstruct.length data >= header_size) (`Msg "too small") in
  let src_port = Cstruct.BE.get_uint16 data 0
  and dst_port = Cstruct.BE.get_uint16 data 2
  and seq = Sequence.of_int32 (Cstruct.BE.get_uint32 data 4)
  and ack = Sequence.of_int32 (Cstruct.BE.get_uint32 data 8)
  and data_off = (Cstruct.get_uint8 data 12 lsr 4) * 4 (* lower 4 are reserved [can't assume they're 0] *)
  in
  let* ackf, flag, push = Flag.decode (Cstruct.get_uint8 data 13) in
  let window = Cstruct.BE.get_uint16 data 14
  and checksum = Cstruct.BE.get_uint16 data 16
  (* and _up = Cstruct.BE.get_uint16 data 18 *)
  in
  let* () =
    guard (Cstruct.length data >= data_off) (`Msg "data_offset too big")
  in
  let options_buf = Cstruct.sub data header_size (data_off - header_size) in
  let* options = decode_options options_buf in
  let payload = Cstruct.shift data data_off in
  let ack = if ackf then Some ack else None in
  Ok ({ src_port; dst_port; seq; ack; flag; push; window; options; payload },
      checksum)

let decode_and_validate ~src ~dst data =
  let* t, pkt_csum = decode data in
  let computed = checksum ~src ~dst data in
  (* these are already checks done in deliver_in_4, etc. *)
  let pkt_csum = if pkt_csum = 0xffff then 0x0 else pkt_csum in
  let* () =
    guard (computed = pkt_csum)
      (`Msg (Fmt.str "invalid checksum: computed 0x%04X, decoded 0x%04X in src %a dst %a@.%a"
               computed pkt_csum
               Ipaddr.pp src Ipaddr.pp dst
               Cstruct.hexdump_pp data))
  in
  let* () =
    match src, dst with
    | Ipaddr.V4 src, Ipaddr.V4 dst ->
      let* () =
        guard Ipaddr.V4.(compare src broadcast <> 0)
          (`Msg "segment from broadcast")
      in
      guard Ipaddr.V4.(compare dst broadcast <> 0)
        (`Msg "segment to broadcast")
    | _ -> Ok ()
  in
  let* () =
    guard (not (Ipaddr.is_multicast src))
      (`Msg "segment from multicast address")
  in
  let* () =
    guard (not (Ipaddr.is_multicast dst))
      (`Msg "segment to multicast address")
  in
  let* () =
    guard (not ((Ipaddr.compare src dst = 0 && t.src_port = t.dst_port)))
      (`Msg "segment source and destination ip and port are equal")
  in
  Ok (t, to_id ~src ~dst t)
