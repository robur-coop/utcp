(* (c) 2017-2019 Hannes Mehnert, all rights reserved *)

let src = Logs.Src.create "tcp.input" ~doc:"TCP input"
module Log = (val Logs.src_log src : Logs.LOG)

open State

let guard p e = if p then Ok () else Error e

let ( let* ) = Result.bind

(* input rules from netsem
deliver_in_1 - passive open (listener, receive SYN) - handle_noconn
deliver_in_1b - drop bad for listening (listener, receive anything else) - handle_noconn
deliver_in_2 - active open (Syn_sent, receive SYN+ACK) - handle_conn
deliver_in_2a - bad or boring, RST or ignore (Syn_sent, receive RST) - handle_conn
deliver_in_2b - simultaneous open (Syn_sent, receive SYN) - handle_conn
deliver_in_3 - data, fin, ack in established - handle_conn
deliver_in_3a - data with invalid checksum - validate_segment fails
deliver_in_3b - data when process gone away - not handled
deliver_in_3c - stupid ack or land in (Syn_received + bad ACK) - handle_conn and validate_segment fails
deliver_in_3d - valid ack in (Syn_received + ACK -- normal 3WS) - handle_conn
????deliver_in_3y/z - fast path (Established, header prediction, (y) seq = rcv_nxt and data [delack] OR (z) ack [window update/opened ~> may send data])
deliver_in_4 - drop non-sane or martian segment - validate_segment fails
deliver_in_5 - drop with RST sth not matching any socket - handle_noconn
deliver_in_6 - drop sane segment in CLOSED - not handled (no CLOSED, handle_noconn may reset)
deliver_in_7 - recv RST and zap non-CLOSED/LISTEN/SYN_SENT/SYN_RECEIVED/TIME_WAIT - handle_conn
deliver_in_7a - recv RST and zap SYN_RECEIVED state
deliver_in_7b - recv RST and ignore in LISTEN
deliver_in_7c - recv RST and ignore in SYN_SENT/TIME_WAIT
deliver_in_7d - recv RST and zap SYN_SENT
deliver_in_8 - recv SYN in yy - handle_conn
deliver_in_9 - recv SYN in TIME_WAIT (in case there's no LISTEN) - not handled
??deliver_in_10 - stupid flag combinations are dropped (without reset)
*)

let dropwithreset (src, _, dst, _) seg =
  Option.map (fun x -> src, dst, x) (Segment.dropwithreset seg)

let deliver_in_1 mk_notify m stats rng now id seg =
  (* there's a difference from the model, namely that we don't care about
     sockets in TIME_WAIT - this is handled in handle_conn explicitly (allowing
     port reusage with strictly higher sequence numbers - RFC1122 4.2.2.13).

     this rule carries less complexity in this way, and is called via
     handle_noconn (if there's no flow in the connection table),
     or via handle_conn if in time_wait, and a syn, and the seq > rcv_nxt. *)
  m "deliver-in-1";
  let conn =
    let advmss = Subr.tcp_mssopt id in
    let rcvbufsize, sndbufsize, t_maxseg', snd_cwnd' =
      let bw_delay_product_for_rt = None in
      Subr.calculate_buf_sizes advmss (Segment.mss seg)
        bw_delay_product_for_rt Params.so_rcvbuf Params.so_sndbuf
    in
    let rcv_wnd = rcvbufsize in
    let tf_doing_ws, snd_scale =
      match Segment.ws seg with
      | Some x when x <= Params.tcp_maxwinscale -> true, x
      | _ -> false, 0
    in
    let request_r_scale, rcv_scale =
      if tf_doing_ws then Some Params.scale, Params.scale else None, 0
    in
    let iss = Sequence.of_int32 (Randomconv.int32 rng)
    and ack' = Sequence.incr seg.Segment.seq (* ACK the SYN *)
    in
    let t_rttseg = Some (now, iss) in
    let control_block = {
      initial_cb with
      tt_rexmt = Subr.start_tt_rexmt now 0 false initial_cb.t_rttinf ;
      t_idletime = now ;
      iss ;
      irs = seg.Segment.seq ;
      rcv_wnd = rcvbufsize ;
      tf_rxwin0sent = (rcv_wnd = 0) ;
      rcv_adv = Sequence.addi ack' rcv_wnd ;
      rcv_nxt = ack' ;
      snd_una = iss ;
      snd_max = Sequence.incr iss ;
      snd_nxt = Sequence.incr iss ;
      snd_cwnd = snd_cwnd' ;
      t_maxseg = t_maxseg' ;
      t_advmss = advmss ;
      tf_doing_ws ; snd_scale ; rcv_scale ;
      request_r_scale ;
      last_ack_sent = ack' ;
      t_rttseg }
    in
    conn_state now mk_notify ~rcvbufsize ~sndbufsize Syn_received control_block
  in
  let reply = Segment.make_syn_ack conn.control_block id in
  Log.debug (fun m -> m "%a passive open %a" Connection.pp id (pp_conn_state now) conn);
  Stats.incr_passive stats;
  conn, reply

let deliver_in_2 m stats now id conn seg ack =
  m "deliver-in-2";
  let cb = conn.control_block in
  let* () = guard (Sequence.equal ack cb.snd_nxt) (`Drop (fun () -> "ack = snd_nxt")) in
  let tf_doing_ws, snd_scale, rcv_scale =
    match Segment.ws seg, cb.request_r_scale with
    | None, _ -> false, 0, 0
    | Some x, Some y -> true, x, y
    | Some x, None ->
      (* may our 3rd time retransmitted SYN hits them, and we don't know what
         we sent (use Params.scale for now) *)
      true, x, Params.scale
  in
  let rcvbufsize, sndbufsize, t_maxseg, snd_cwnd =
    let bw_delay_product_for_rt = None in
    Subr.calculate_buf_sizes cb.t_advmss (Segment.mss seg) bw_delay_product_for_rt
      conn.rcvbufsize conn.sndbufsize
  in
  let rcv_wnd = Subr.calculate_bsd_rcv_wnd conn in
  let t_softerror, t_rttseg, t_rttinf, tt_rexmt =
    (*: update RTT estimators from timestamp or roundtrip time :*)
    let emission_time = match cb.t_rttseg with
      | Some (ts0, seq0) when Sequence.greater ack seq0 -> Some ts0
      | _ -> None
    in
    (*: clear soft error, cancel timer, and update estimators if we successfully timed a segment round-trip :*)
    let t_softerror', t_rttseg', t_rttinf' =
      match emission_time with
      | Some ts -> None, None, Subr.update_rtt (Mtime.span now ts) cb.t_rttinf
      | _ ->
        cb.t_softerror, cb.t_rttseg, cb.t_rttinf
    in
    (*: mess with retransmit timer if appropriate :*)
    let tt_rexmt' =
      if Sequence.equal ack cb.snd_max then
        (*: if acked everything, stop :*)
        None
        (*: [[needoutput = 1]] -- see below :*)
      else match cb.tt_rexmt with
        | Some ((RexmtSyn, _), _) ->
          (*: if partial ack, restart from current backoff value,
              which is always zero because of the above updates to
              the RTT estimators and shift value. :*)
          Subr.start_tt_rexmt_syn now 0 true t_rttinf'
        | None | Some ((Rexmt, _), _) ->
          (*: ditto :*)
          Subr.start_tt_rexmt now 0 true t_rttinf'
        | Some ((Persist, _), _) when emission_time <> None ->
          (* hannes 2019-07-15 looked a bit longer at the further conditionals,
             and Persist when emission_time was the only case left (and now its
             very symmetric...) *)
          Subr.start_tt_persist now 0 t_rttinf'
        | c -> c
    in
    t_softerror', t_rttseg', t_rttinf', tt_rexmt'
  in
  let rcv_nxt = Sequence.incr seg.seq in
  let control_block = {
    cb with
    tt_rexmt ;
    t_idletime = now ;
    tt_conn_est = None ;
    tt_delack = None ;
    snd_una = Sequence.incr cb.iss ; (*: ack ; = cb.iss + 1, or +2 if full ack of SYN,FIN :*)
    snd_nxt = if conn.cantsndmore then ack else cb.snd_nxt ;
    snd_max = if conn.cantsndmore && Sequence.greater ack cb.snd_max then ack else cb.snd_max ;
    snd_wl1 = Sequence.incr seg.seq ;
    snd_wl2 = ack ;
    snd_wnd = seg.window ; (* this is a SYN segment, so window scaling is ignored *)
    snd_cwnd ;
    rcv_scale ;
    snd_scale ;
    tf_doing_ws ;
    irs = seg.seq ;
    rcv_nxt ;
    rcv_wnd ;
    tf_rxwin0sent = (rcv_wnd = 0) ;
    rcv_adv = Sequence.addi rcv_nxt ((Int.min (rcv_wnd lsr rcv_scale) Params.tcp_maxwin) lsl rcv_scale) ;
    t_maxseg ;
    last_ack_sent = rcv_nxt ;
    t_softerror ;
    t_rttseg ;
    t_rttinf ;
  }
  in
  Stats.incr_established stats;
  Ok ({ conn with control_block; tcp_state = Established; rcvbufsize; sndbufsize },
      Segment.make_ack control_block ~fin:false id)

let deliver_in_2a m conn seg f =
  m "deliver-in-2a";
  (* well well, the remote could have leftover state and send us a ack+fin... but that's fine to drop (and unlikely to happen now that we have random)
     server.exe: [DEBUG] 10.0.42.2:20 -> 10.0.42.1:1234 handle_conn TCP syn sent cb snd_una 0 snd_nxt 1 snd_wl1 0 snd_wl2 0 iss 0 rcv_wnd 65000 rcv_nxt 0 irs 0 seg AF seq 3062921918 ack 1 window 65535 opts 0 bytes data
     server.exe: [ERROR] dropping segment in syn sent failed condition RA *)
  match f, seg.Segment.ack with
  | Some `Rst, Some ack ->
    if Sequence.equal ack conn.control_block.snd_nxt then
      Ok ()
    else
      Error (`Drop (fun () -> "ACK in-window"))
  | _ -> Error (`Drop (fun () -> "RA"))

let deliver_in_3c_3d m stats now conn seg =
  m "deliver-in-3c-3d";
  (* deliver_in_3c and syn_received parts of deliver_in_3 (now deliver_in_3d) *)
  (* TODO hostLTS:15801: [[SYN]] flag set may be set in the final segment of a
     simultaneous open (does this change anything for us?) *)
  (* yes: seq may be cb.irs and flags = syn+ack (simultaneous open!) *)
  let cb = conn.control_block in
  (* what is the current state? *)
  (* - we acked the initial syn, their seq should be rcv_nxt (or?) *)
  (* - furthermore, it should be >= irs -- that's redundant with above *)
  (* if their seq is good (but their ack isn't or it is no ack), reset *)
  let* () =
    guard (Sequence.equal seg.Segment.seq cb.rcv_nxt) (`Drop (fun () -> "seq = rcv_nxt"))
  in
  (* - we sent our syn, so we expect an appropriate ack for the syn! *)
  (* - we didn't send out more data, so that ack should be exact *)
  (* if their seq is not good, drop packet -- TODO allow syn *)
  match seg.Segment.flag, seg.Segment.ack with
  | Some `Rst, _ -> Error (`Reset (fun () -> "received valid reset"))
  | _, None -> Error (`Drop (fun () -> "ACK flag"))
  | Some _, _ -> Error (`Drop (fun () -> "S|F"))
  | _, Some ack ->
    (* hostLTS:15828 - well, more or less ;) *)
    (* auxFns:2252 ack < snd_una || snd_max < ack -> break LAND DoS, prevent ACK storm *)
    (* TODO what is an acceptable ack? snd_nxt, if <> what to do? *)
    let* () = guard (Sequence.equal ack cb.snd_nxt) (`Reset (fun () -> "ack = snd_nxt")) in
    (* not (ack <= tcp_sock.cb.snd_una \/ ack > tcp_sock.cb.snd_max) *)
    (* TODO rtt measurement likely *)
    (* expect (assume for now): no data in that segment !? *)
    let control_block = {
      cb with snd_una = ack ;
              snd_wnd = seg.Segment.window lsl cb.snd_scale ;
              snd_wl1 = seg.Segment.seq ; (* need to check with model, from RFC1122 4.2.2.20 *)
              snd_wl2 = ack ;
              t_idletime = now ;
    } in
    (* if not cantsendmore established else if ourfinisacked fin_wait2 else fin_wait_1 *)
    Stats.incr_established stats;
    Ok { conn with control_block ; tcp_state = Established }

let in_window cb seg =
  (* from table in 793bis13 3.3 *)
  let seq = seg.Segment.seq
  and max = Sequence.addi cb.rcv_nxt cb.rcv_wnd
  in
  match seg.Segment.payload_len, cb.rcv_wnd with
  | 0, 0 -> Sequence.equal seq cb.rcv_nxt
  | 0, _ -> Sequence.less_equal cb.rcv_nxt seq && Sequence.less seq max
  | _, 0 -> false
  | dl, _ ->
    let rseq = Sequence.addi seq (pred dl) in
    (Sequence.less_equal cb.rcv_nxt seq && Sequence.less seq max) ||
    (Sequence.less_equal cb.rcv_nxt rseq && Sequence.less rseq max)
(* TODO are we missing (from di3_topstuff) the window probe
   ("segment_off_right_hand_edge" and rcv_wnd' is <> 0) *)

let di3_topstuff now conn =
  (* we're not doing PAWS (no timestamp), and already checked in_window *)
  let rcv_wnd = Subr.calculate_bsd_rcv_wnd conn in
  let cb = conn.control_block in
  let t_idletime = now
  and tt_fin_wait_2 =
    Option.map (fun _ -> Timers.timer now () Params.tcptv_maxidle)
      cb.tt_fin_wait_2
  in
  { cb with t_idletime ; tt_fin_wait_2 ; rcv_wnd }

(* this is more or less andThen! (no bndlm though) *)
let ( >>>= ) ((conn, out), cont) f =
  if cont then
    let (conn', out'), cont' = f conn in
    (conn', out @ out'), cont'
  else
    (conn, out), cont

let di3_newackstuff now id conn ourfinisacked ack =
  (*: Pull some fields out of the segment :*)
  let cb = conn.control_block in
  let conn', out =
    if cb.t_dupacks < 3 then
      (*: If there have been fewer than 3 duplicate [[ACKS]] then clear the
         duplicate [[ACK]] counter. If there were more than 3 duplicate [[ACKS]]
         previously then the congestion window was inflated as per RFC2581 so
         retract it to [[snd_ssthresh]]
        -- hannes, 20231027: not needed to retract cwnd, since we do new reno,
           and thus t_dupacks will never be >= 3 here :*)
      let control_block = { cb with t_dupacks = 0 } in
      { conn with control_block }, []
    else if cb.t_dupacks >= 3 && Sequence.less ack cb.snd_recover then
      (*: The host supports NewReno-style Fast Recovery, the socket has received
         at least three duplicate [[ACK]]s previously and the new [[ACK]] does
         not complete the recovery process, \ie, there are further losses or
         network delays. The new [[ACK]] is a partial [[ACK]] per
         RFC2582. Perform a retransmit of the next unacknowledged segment and
         deflate the congestion window as per the RFC. :*)
      let snd_nxt' = cb.snd_nxt in
      let control_block = {
        cb with
        (*: Clear the retransmit timer and round-trip time measurement
          timer. These will be started by [[tcp_output_really]] when the
          retransmit is actioned. :*)
        tt_rexmt = None;
        t_rttseg = None;
        (*: Segment to retransmit starts here :*)
        snd_nxt = ack;
        (*: Allow one segment to be emitted :*)
        (* TODO hannes 2019-07-19 ABC? *)
        snd_cwnd = cb.t_maxseg
      } in
      (* Attempt to create a segment for output using the modified control block *)
      let conn', out =
        Segment.tcp_output_perhaps now id { conn with control_block }
      in
      (*: Finally update the control block:  :*)
      let cb' = conn'.control_block in
      let control_block = {
        cb' with
        (*: RFC2582 partial window deflation: deflate the congestion window by
           the amount of data freshly acknowledged and add back one maximum
           segment size :*)
        snd_cwnd = cb'.snd_cwnd - Sequence.window ack cb'.snd_una + cb'.t_maxseg;
        snd_nxt = snd_nxt' (*: restore previous value :*)
      } in
      { conn' with control_block }, out
    else if cb.t_dupacks >= 3 && Sequence.greater_equal ack cb.snd_recover then
      (*: The host supports NewReno-style Fast Recovery, the socket has received
         at least three duplicate [[ACK]] segments and the new [[ACK]]
         acknowledges at least everything upto [[snd_recover]], completing the
         recovery process. :*)
      let snd_cwnd =
        if Sequence.window cb.snd_max ack < cb.snd_ssthresh then
          (*: If [[snd_ssthresh]] is greater than the number of bytes of data
             still unacknowledged and presumed to be in-flight, set [[snd_cwnd]]
             to be one segment larger than the total size of all the segments in
             flight. This is burst avoidance: [[tcp_output]] is only able to
             send upto one further segment until some of the in flight data is
             acknowledged. :*)
          Sequence.window cb.snd_max ack + cb.t_maxseg
        else
          (*: Otherwise, set [[snd_cwnd]] to be [[snd_ssthresh]], forbidding any
             further segment output until some in flight data is
             acknowledged.:*)
          cb.snd_ssthresh
      in
      let control_block = {
        cb with
        t_dupacks = 0; (*: clear the duplicate [[ACK]] counter :*)
        (*: Open up the congestion window, being careful to avoid an RFC2582
           Ch3.5 Pg6 "burst of data". :*)
        snd_cwnd
      } in
      { conn with control_block }, []
    else
      invalid_arg "di3_newackstuff" (*: impossible :*)
  in
  (*: Check [[ack]] value is sensible, \ie, not greater than the highest
     sequence number transmitted so far :*)
  if Sequence.greater ack cb.snd_max then
    (*: Drop the segment and possibly emit a [[RST]] segment :*)
    (* hannes 2019-07-19 dropafterack used to be called, which does:
       - if state = SYN_RCVD and ACK and ack < snd_una || snd_max < ack
         ~> dropwithreset (break loop in LAND, prevent ACK storm from two
            listening ports that have been sent forged SYN segments)
         --> we're already post-SYN_RCVD here (3c_3d handles SYN_RCVD)
       - else: tcp_output_really arch F ticks ifds sock (sock1,[msg])
         so, this is very similar to a challenge ack, no? *)
    let conn'', out' = Segment.tcp_output_really now id false conn' in
    (Some conn'', out @ [ out' ]), false
  else (*: continue processing :*)
    (*: If the retransmit timer is set and the socket has done only one
       retransmit and it is still within the bad retransmit timer window, then
       because this is an [[ACK]] of new data the retransmission was done in
       error. Flag this so that the control block can be recovered from
       retransmission mode. This is known as a "bad retransmit". :*)
    let revert_rexmt =
      (match cb.tt_rexmt with
       | Some ((Rexmt, 1), _) | Some ((RexmtSyn, 1), _) -> true | _ -> false)
      (* /\ timewindow_open cb'.t_badrxtwin) *)
    in
    (*: Attempt to calculate a new round-trip time estimate :*)
    let emission_time = match cb.t_rttseg with
      | Some (ts0,seq0) ->
        (*: Or if not, by the control blocks round-trip timer, if it covers the
           segment(s) being acknowledged :*)
        if Sequence.greater ack seq0 then Some ts0 else None
      | None ->
        (*: Otherwise, it is not possible to calculate a round-trip update :*)
        None
    in
    (*: If a new round-trip time estimate was calculated above, update the round-trip information
        held by the socket's control block :*)
    let t_rttinf' = match emission_time with
      | Some ts -> Subr.update_rtt (Mtime.span now ts) cb.t_rttinf
      | None -> cb.t_rttinf
    in
    (*: Update the retransmit timer :*)
    let tt_rexmt' =
      if Sequence.equal ack cb.snd_max then
        None (*: If all sent data has been acknowledged, disable the timer :*)
      else match mode_of cb.tt_rexmt with
        | None ->
          (*: If not set, set it as there is still unacknowledged data :*)
          Subr.start_tt_rexmt now 0 true t_rttinf'
        | Some Rexmt ->
          (*: If set, reset it as a new acknowledgement segment has arrived :*)
          Subr.start_tt_rexmt now 0 true t_rttinf'
        | _ ->
          (*: Otherwise, leave it alone. The timer will never be in [[RexmtSyn]]
             here and the only other case is [[Persist]], in which case it
             should be left alone until such time as a window update is received
             :*)
          cb.tt_rexmt
    in
    (*: Update the send queue and window :*)
    let snd_wnd', sndq =
      if ourfinisacked then
        (*: If this socket has previously emitted a [[FIN]] segment and the
           [[FIN]] has now been [[ACK]]ed, decrease [[snd_wnd]] by the length of
           the send queue and clear the send queue.:*)
        cb.snd_wnd - Rope.length conn.sndq, Rope.empty
      else
        (*: Otherwise, reduce the send window by the amound of data acknowledged
           as it is now consuming space on the receiver's receive queue. Remove
           the acknowledged bytes from the send queue as they will never need to
           be retransmitted.:*)
        let acked = Sequence.window ack cb.snd_una in
        cb.snd_wnd - acked,
        Rope.shift conn.sndq acked
    in
    (*: Update the control block :*)
    let cb' =
      if revert_rexmt then
        (*: If [[revert_rexmt]] (above) flags that a bad retransmission occured,
           undo the congestion avoidance changes :*)
        let cb = conn'.control_block in
        { cb with
          snd_cwnd = cb.snd_cwnd_prev ;
          snd_ssthresh = cb.snd_ssthresh_prev ;
          snd_nxt = cb.snd_max ;
          (* t_badrxtwin = TimeWindowClosed ; *)
        }
      else
        conn'.control_block
    in
    let t_softerror, t_rttseg =
      (*: If the [[ACK]] segment allowed us to successfully time a segment (and
         update the round-trip time estimates) then clear the soft error flag
         and clear the segment round-trip timer in order that it can be used on
         a future segment. :*)
      match emission_time with
      | None -> cb'.t_softerror, cb'.t_rttseg
      | Some _ -> None, None
    and snd_cwnd =
      (*: Update the congestion window by the algorithm in {@link
         [[expand_cwnd]]} only when not performing NewReno retransmission or the
         duplicate [[ACK]] counter is zero, \ie, expand the congestion window
         when this [[ACK]] is not a NewReno-style partial [[ACK]] and hence the
         connection has yet recovered :*)
      if cb'.t_dupacks = 0 then
        (* TODO cb unclear, some used to be tcp_sock0.cb *)
        Subr.expand_cwnd cb.snd_ssthresh cb.t_maxseg
          (Params.tcp_maxwin lsl cb.snd_scale) cb.snd_cwnd
      else
        cb'.snd_cwnd
    and tt_2msl =
      (*: Reset the [[2MSL]] timer if in the [[TIME_WAIT]] state as have
         received a valid [[ACK]] segment for the waiting socket :*)
      match conn'.tcp_state with
      | Time_wait -> Some (Timers.timer now () (Int64.shift_left Params.tcptv_msl 1))
      | _ -> cb'.tt_2msl (* should be equivalent to None *)
    in
    let cb'' = {
      cb' with
      (*: Update the round-trip time estimates and retransmit timer :*)
      t_rttinf = t_rttinf';
      tt_rexmt = tt_rexmt';
      t_softerror ;
      t_rttseg ;
      snd_cwnd ;
      snd_wnd = snd_wnd'; (*: The updated send window :*)
      snd_una = ack; (*: Have had up to [[ack]] acknowledged :*)
      snd_nxt = max ack cb'.snd_nxt ; (*: Ensure invariant [[snd_nxt >= snd_una]] :*)
      tt_2msl
    } in
    (*: The send queue update :*)
    let conn'' = { conn' with control_block = cb'' ; sndq } in
    match conn''.tcp_state with
    | Last_ack when ourfinisacked ->
      (* If the socket's [[FIN]] has been acknowledged and the socket is in the
         [[LAST_ACK]] state, close the socket and stop processing this segment *)
      (None, []), false
    | Time_wait when Sequence.greater ack cb.snd_una -> (* hannes check which cb! *)
      (* data acked past FIN *)
      (*: If the socket is in [[TIME_WAIT]] and this segment contains a new
         acknowledgement (that acknowledges past the [[FIN]] segment, drop
         it---it's invalid. Stop processing. :*)
      let conn''', out' = Segment.tcp_output_really now id false conn'' in
      (Some conn''', out @ [ out' ]), false
    | _ ->
      (*: Otherwise, flag that [[deliver_in_3]] can continue processing the
         segment if need be :*)
      (Some conn'', out), true

let di3_ackstuff now id conn seg ourfinisacked fin ack =
  let cb = conn.control_block in
  let win = seg.Segment.window lsl cb.snd_scale in
  (*: The segment is possibly a duplicate ack if it contains no data, does not
     contain a window update and the socket has unacknowledged data (the
     retransmit timer is still active).  The no data condition is important: if
     this socket is sending little or no data at present and is waiting for some
     previous data to be acknowledged, but is receiving data filled segments
     from the other end, these may all contain the same acknowledgement number
     and trigger the retransmit logic erroneously. :*)
  let maybe_dup_ack =
    seg.payload_len = 0 && win = cb.snd_wnd &&
    match cb.tt_rexmt with Some ((Rexmt, _), _) -> true | _ -> false
  in
  (* It turns out since some time the first FIN(+ACK) doesn't account for
     dupacks this is simultaneous close, see rev261244 (and rev239672 and
     rev258821) for details *)
  if
    Sequence.less_equal ack cb.snd_una && maybe_dup_ack && fin &&
    match conn.tcp_state with Close_wait | Closing | Last_ack | Time_wait -> false | _ -> true
  then
    let control_block = { cb with t_dupacks = 0 } in
    (Some { conn with control_block }, []), true
  else if Sequence.less_equal ack cb.snd_una && maybe_dup_ack then
    (*: Received a duplicate acknowledgement: it is an old acknowledgement
       (strictly less than [[snd_una]]) and it meets the duplicate
       acknowledgement conditions above.  Do Fast Retransmit/Fast Recovery
       Congestion Control (RFC2581 Ch3.2 Pg6) and NewReno-style Fast Recovery
       (RFC2582, Ch3 Pg3), updating the control block variables and creating
       segments for transmission as appropriate. :*)
    let t_dupacks' = cb.t_dupacks + 1 in
    if t_dupacks' < 3  then
      (*: Fewer than three duplicate acks received so far. Just increment the
         duplicate ack counter.  We must continue processing, in case [[FIN]] is
         set. :*)
      let control_block = { cb with t_dupacks = t_dupacks' } in
      (Some { conn with control_block }, []), true
    else if t_dupacks' > 3 || (t_dupacks' = 3 && Sequence.less ack cb.snd_recover) then
      (*: If this is the 4th or higher duplicate [[ACK]] then Fast
         Retransmit/Fast Recovery congestion control is already in progress.
         Increase the congestion window by another maximum segment size (as the
         duplicate [[ACK]] indicates another out-or-order segment has been
         received by the other end and is no longer consuming network resource),
         increment the duplicate [[ACK]] counter, and attempt to output another
         segment. :*)
      (*: If this is the 3rd duplicate [[ACK]], the host supports NewReno
         extensions and [[ack]] is strictly less than the fast recovery
         "recovered" sequence number [[snd_recover]], then the host is already
         doing NewReno-style fast recovery and has possibly falsely
         retransmitted a segment, the retransmitted segment has been lost or it
         has been delayed. Reset the duplicate [[ACK]] counter, increase the
         congestion window by a maximum segment size (for the same reason as
         before) and attempt to output another segment. NB: this will not cause
         a cycle to develop! The retransmission timer will eventually fire if
         recovery does not happen "fast". :*)
      let t_dupacks =
        if t_dupacks' = 3 then 0 (* false retransmit, or further loss or delay *)
        else t_dupacks'
      and snd_cwnd = cb.snd_cwnd + cb.t_maxseg
      in
      (* TODO hannes 2019-07-19 increment by cb.t_maxseg changes due to ABC *)
      let control_block = { cb with t_dupacks ; snd_cwnd } in
      let conn' = { conn with control_block } in
      let conn'', out = Segment.tcp_output_perhaps now id conn' in
      (Some conn'', out), false
    else if t_dupacks' = 3 && not (Sequence.less ack cb.snd_recover) then
      (*: If this is the 3rd duplicate segment and if the host supports NewReno
         extensions, a NewReno-style Fast Retransmit is not already in progress,
         then do a Fast Retransmit :*)
      (*: Update the control block before the retransmit to reflect which data
         requires retransmission :*)
      let snd_ssthresh =
        (*: Set to half the current flight size as per RFC2581/2582 :*)
        (* TODO hannes still true in respect of ABC? *)
        Int.max 2 ((Int.min cb.snd_wnd cb.snd_cwnd) / 2 / cb.t_maxseg) * cb.t_maxseg
      in
      let control_block = {
        cb with t_dupacks = t_dupacks' ;
                snd_ssthresh ;
                snd_recover = cb.snd_max ;
                (*: Clear the retransmit timer and round-trip time measurement
                   timer. These will be started by [[tcp_output_really]] when
                   the retransmit is actioned. :*)
                tt_rexmt = None;
                t_rttseg = None;
                (*: Sequence number to retransmit---this is equal to the [[ack]]
                   value in the duplicate [[ACK]] segment :*)
                snd_nxt = ack;
                (*: Ensure the congestion window is large enough to allow one
                   segment to be emitted :*)
                snd_cwnd = cb.t_maxseg
      } in
      (*: Attempt to create a segment for output using the modified control
         block (this is all a relational monad idiom) :*)
      let conn' = { conn with control_block } in
      let conn'', out = Segment.tcp_output_perhaps now id conn' in
      (*: Finally, update the congestion window to [[snd_ssthresh]] plus 3
         maximum segment sizes (this is the artificial inflation of RFC2581/2582
         because it is known that the 3 segments that generated the 3 duplicate
         acknowledgments are received and no longer consuming network
         resource. Also put [[snd_nxt]] back to its previous value. :*)
      let control_block = {
        conn''.control_block with
        snd_cwnd = control_block.snd_ssthresh + cb.t_maxseg * t_dupacks' ;
        snd_nxt = Sequence.max cb.snd_nxt control_block.snd_nxt
      } in
      (Some { conn'' with control_block }, out), false
    else
      invalid_arg "di3_ackstuff" (*: Believed to be impossible---here for completion and safety :*)
  else if Sequence.less_equal ack cb.snd_una && not maybe_dup_ack then
    (*: Have received an old (would use the word "duplicate" if it did not have
       a special meaning) [[ACK]] and it is neither a duplicate [[ACK]] nor the
       [[ACK]] of a new sequence number thus just clear the duplicate [[ACK]]
       counter. :*)
    let control_block = { cb with t_dupacks = 0 } in
    (Some { conn with control_block }, []), true
  else (*: Must be: [[ack > cb.snd_una]] :*)
    (*: This is the [[ACK]] of a new sequence number---this case is handled by
       the auxiliary function {@link [[di3_newackstuff]]} :*)
    di3_newackstuff now id conn ourfinisacked ack

let di3_datastuff_really now the_ststuff conn seg _bsd_fast_path ourfinisacked fin =
  (* hannes 2019-07-19: there used to be let seq = seg.seq + if SYN then 1 else
     0, but we'll never execute this code with a segment that has SYN *)
  (* hannes 2023-08-28: this used to compute the sender's advertised window and
     shift by scale, but that value was never used. *)
  let cb = conn.control_block in
  (*: Trim segment to be within the receive window :*)
  (*: Trim duplicate data from the left edge of [[data]], \ie, data before
     [[cb.rcv_nxt]].  Adjust [[seq]], [[URG]] and [[urp]] in respect of left
     edge trimming. If the urgent data has been trimmed from the segment's data,
     [[URG]] is cleared also.  Note: the urgent pointer always points to the
     byte immediately following the urgent byte and is relative to the start of
     the segment's data. An urgent pointer of zero signifies that there is no
     urgent data in the segment. :*)
  let trim_amt_left =
    if Sequence.greater cb.rcv_nxt seg.Segment.seq then
      Int.min (Sequence.window cb.rcv_nxt seg.seq) seg.payload_len
    else
      0
  in
  let data_trimmed_left = Rope.of_strings seg.payload in
  let data_trimmed_left = Rope.shift data_trimmed_left trim_amt_left in
  let seq_trimmed = Sequence.addi seg.seq trim_amt_left in
  (*: Trimmed data starts at [[seq_trimmed]] :*)
  (*: Trim any data outside the receive window from the right hand edge. If all
     the data is within the window and the [[FIN]] flag is set then the [[FIN]]
     flag is valid and should be processed.  Note: this trimming may remove
     urgent data from the segment. The urgent pointer and flag are not cleared
     here because there is still urgent data to be received, but now in a future
     segment. :*)
  let data_trimmed_left_right =
    let len = Int.min cb.rcv_wnd (Rope.length data_trimmed_left) in
    Rope.chop data_trimmed_left len
  in
  let fin_trimmed =
    fin && Rope.length data_trimmed_left_right == Rope.length data_trimmed_left
  in
  (*: Build trimmed segment to place on reassembly queue.  If urgent data is in
     this segment and the socket is not doing inline delivery (and hence the
     urgent byte is stored in [[iobc]]), remove the urgent byte from the
     segment's data so that it does not get placed in the receive queue, and set
     [[spliced_urp]] to the sequence number of the urgent byte. :*)
  (*: Processing of non-urgent data. There are 6 cases to consider: :*)
  (*: Case (1) The segment contains new in-order, in-window data possibly with a
     [[FIN]] and the receive window is not closed. Note: it is possible that the
     segment contains just one byte of OOB data that may have already been
     pulled out into [[iobc]] if OOB delivery is out-of-line. In which case, the
     below must still be performed even though no data is contributed to the
     reassembly buffer in order that [[rcv_nxt]] is updated correctly (because a
     byte of urgent data consumes a byte of sequence number space). This is why
     [[data_trimmed_left_right]] is used rather than [[data_deoobed]] in some of
     the conditions below. :*)
  let rseq_trimmed =
    Sequence.addi seq_trimmed
      (Rope.length data_trimmed_left_right + (if fin_trimmed then 1 else 0))
  in
  let (conn', fin_reass, out), cont =
    if
      Sequence.equal seq_trimmed cb.rcv_nxt &&
      Sequence.greater rseq_trimmed cb.rcv_nxt &&
      cb.rcv_wnd > 0
    then
      (*: Only need to acknowledge the segment if there is new in-window data
         (including urgent data) or a valid [[FIN]] :*)
      let have_stuff_to_ack =
        Rope.length data_trimmed_left_right > 0 || fin_trimmed
      in
      (*: If the socket is connected, has data to [[ACK]] but no [[FIN]] to
         [[ACK]], the reassembly queue is empty, the socket is not currently
         within a bad retransmit window and an [[ACK]] is not already being
         delayed, then delay the [[ACK]]. :*)
      let delay_ack =
        is_connected conn.tcp_state &&
        have_stuff_to_ack && not fin_trimmed && Reassembly_queue.is_empty cb.t_segq &&
        not cb.tf_rxwin0sent && cb.tt_delack = None
      in
      (*: Check to see whether any data or a [[FIN]] can be
          reassembled. hannes (2023-08-30 nothing non-deterministically here,
          also not dealing with oob (as does this stack) :*)
      let t_segq, r = Reassembly_queue.maybe_take cb.t_segq rseq_trimmed in
      (* Length (in sequence space) of reassembled data, counting a [[FIN]] as
         one byte and including any out-of-line urgent data previously removed *)
      let data_reass, fin_reass0 = Option.value ~default:(Rope.empty, false) r in
      let data = Rope.concat data_trimmed_left_right data_reass in
      let fin_reass_trimmed = fin_trimmed || fin_reass0 in
      let data_len = Rope.length data + if fin_reass_trimmed then 1 else 0 in
     (*: Add the reassembled data to the receive queue and increment [[rcv_nxt]]
        to mark the sequence number of the byte past the last byte in the
        receive queue:*)
      (*: Prune the receive queue of any data or [[FIN]]s that were reassembled,
         keeping all segments that contain data at or past sequence number
         [[cb.rcv_nxt + len_reass]]. :*)
      (*: Reduce the receive window in light of the data added to the receive
         queue. Do not include out-of-line urgent data because it does not store
         data in the receive queue. :*)
     (*: Hack: assertion used to share values with later conditions :*)
      (* assert (FIN_reass = FIN_reass0) andThen *)
      (*: Update the socket state :*)
      let tt_delack =
        (*: Start the delayed ack timer if decided to earlier, \ie, [[delay_ack = T]]. :*)
        if delay_ack then Some (Timers.timer now () Params.tcptv_delack) else None
      and tf_shouldacknow =
        (*: Set if not delaying an [[ACK]] and have stuff to [[ACK]] :*)
        not delay_ack && have_stuff_to_ack
      and rcv_nxt = Sequence.addi cb.rcv_nxt data_len
      and rcv_wnd = cb.rcv_wnd - (Rope.length data)
      in
      let control_block = {
        cb with
        tt_delack ;
        tf_shouldacknow ;
        t_segq ;   (*: updated reassembly queue, post-pruning :*)
        rcv_nxt ;
        rcv_wnd ;
      }
      and rcvq = Rope.concat conn.rcvq data
      in
      ({ conn with control_block ; rcvq }, fin_reass_trimmed, []), true
     (*: Case (2) The segment contains new out-of-order in-window data, possibly
        with a [[FIN]], and the receive window is not closed. Note: it may also
        contain in-window urgent data that may have been pulled out-of-line but
        still require processing to keep reassembly happy. :*)
    else if
      Sequence.greater seq_trimmed cb.rcv_nxt &&
      Sequence.less seq_trimmed (Sequence.addi cb.rcv_nxt cb.rcv_wnd) &&
      Rope.length data_trimmed_left_right + (if fin_trimmed then 1 else 0) > 0 &&
      cb.rcv_wnd > 0
    then
      (*: Hack: assertion used to share values with later conditions :*)
      let fin_reass = false in (* it is out-of-order *)
      (*: Update the socket's TCP control block state :*)
      let t_segq =
        Reassembly_queue.insert_seg cb.t_segq (seq_trimmed, fin_trimmed, data_trimmed_left_right)
      in
      let control_block = { cb with tf_shouldacknow = true ; t_segq } in
      (* since it is out-of-order, we do not (yet) handle the fin *)
      ({ conn with control_block }, fin_reass, []), true
      (*: Case (3) The segment is a pure [[ACK]] segment (contains no data) (and
         must be in-order). :*)
      (*: Invariant here that [[seq_trimmed = seq]] if segment is a pure
         [[ACK]]. Note: the length of the original segment (not the trimmed
         segment) is used in the guard to ensure this really was a pure [[ACK]]
         segment. :*)
    else if Sequence.equal seq_trimmed cb.rcv_nxt &&
            seg.payload_len + (if fin then 1 else 0) = 0
    then
      (*: Hack: assertion used to share values with later conditions :*)
      let fin_reass = false in (* Have not received a FIN *)
      (conn, fin_reass, []), true
      (*: Case (4) Segment contained no useful data---was a completely old
         segment. Note: the original fields from the segment, \ie, [[seq]],
         [[data]] and [[FIN]] are used in the guard below---the trimmed variants
         are useless here! :*)
      (*: Case (5) Segment is a window probe.  Note: the original fields from
         the segment, \ie, [[data]] and [[FIN]] are used in the guard
         below---the trimmed variants are useless here! :*)
      (*: Case (6) Segment is completely beyond the window and is not a window
         probe :*)
    else
      (* hannes 2023-08-29 since the last case is "true", skip the conditional.
         no need to evaluate any conditions with a "|| true" at the end *)
    (* if
       (Sequence.less seg.seq cb.rcv_nxt &&
       Sequence.less_equal (Sequence.addi seg.seq (Cstruct.length seg.payload + if fin_trimmed then 1 else 0)) cb.rcv_nxt) || (* (4) *)
       (Sequence.equal seq_trimmed cb.rcv_nxt && cb.rcv_wnd = 0 &&
       Cstruct.length seg.payload + (if fin then 1 else 0) > 0) || (* (5) *)
       true (* uhm, really? (6) *)
       then *)
      (*: Update socket's control block to assert that an [[ACK]] segment should be sent now. :*)
      (*: Source: TCPIPv2p959 says "segment is discarded and an ack is sent as a reply" :*)
      (*: Hack: assertion used to share values with later conditions :*)
      let fin_reass = false in (* Definitely false---segment is outside window *)
      let control_block = { cb with tf_shouldacknow = true } in
      ({ conn with control_block }, fin_reass, []), true
  in
  (*: Finished processing the segment's data :*)
  (*: Thread the reassembled [[FIN]] flag through to [[di3_ststuff]] :*)
  if cont then
    the_ststuff now conn' fin_reass ourfinisacked, out
  else
    conn', out

let di3_datastuff now the_ststuff conn seg ourfinisacked fin ack =
  let cb = conn.control_block in
  let win = seg.Segment.window lsl cb.snd_scale in
  (*: Various things do not happen if BSD processes the segment using its header
     prediction (fast-path) code. Header prediction occurs only in the
     [[ESTABLISHED]] state, with segments that have only [[ACK]] and/or [[PSH]]
     flags set, are in-order, do not contain a window update, when data is not
     being retransmitted (no congestion is occuring) and either:
         (a) the segment is a valid pure ACK segment of new data, less than
     three duplicate [[ACK]]s have been received and the congestion window is at
     least as large as the send window, or
         (b) the segment contains new data, does not acknowlegdge any new data,
     the segment reassembly queue is empty and there is space for the segment's
     data in the socket's receive buffer.  :*)
  let bsd_fast_path =
    (match conn.tcp_state with Established -> true | _ -> false) &&
    not fin &&
    Sequence.equal seg.seq cb.rcv_nxt &&
    cb.snd_wnd = win &&
    Sequence.equal cb.snd_max cb.snd_nxt &&
    ((Sequence.greater ack cb.snd_una && Sequence.less_equal ack cb.snd_max &&
      cb.snd_cwnd >= cb.snd_wnd && cb.t_dupacks < 3)
     || (Sequence.equal ack cb.snd_una && Reassembly_queue.is_empty cb.t_segq &&
         seg.payload_len < conn.rcvbufsize - Rope.length conn.rcvq))
  in
  (*: Update the send window using the received segment if the segment will not be processed by
      BSD's fast path, has the [[ACK]] flag set, is not to the right of the window, and either:
        (a) the last window update was from a segment with sequence number less than [[seq]],
            \ie, an older segment than the current segment, or
        (b) the last window update was from a segment with sequence number equal to [[seq]] but
            with an acknowledgement number less than [[ack]], \ie, this segment acknowledges
            newer data than the segment the last window update was taken from, or
        (c) the last window update was from a segment with sequence number equal to
            [[seq]] and acknowledgement number equal to [[ack]], \ie, a segment similar to that
            the previous update came from, but this segment contains a larger window advertisment
            than was previously advertised, or
        (d) this segment is the third segment during connection establishement (state is
            [[SYN_RECEIVED]]) and does not have the [[FIN]] flag set. :*)
  let update_send_window =
    not bsd_fast_path &&
    Sequence.less_equal seg.seq (Sequence.addi cb.rcv_nxt cb.rcv_wnd) &&
    (Sequence.less cb.snd_wl1 seg.seq ||
     (Sequence.equal cb.snd_wl1 seg.seq &&
      (Sequence.less cb.snd_wl2 ack || Sequence.equal cb.snd_wl2 ack && win > cb.snd_wnd)))
  in
  let seq_trimmed =
    Sequence.max seg.seq (Sequence.min cb.rcv_nxt (Sequence.addi seg.seq seg.payload_len))
  in
  (*: Write back the window updates :*)
  let control_block =
    if update_send_window then
      { cb with
        snd_wnd = win ;
        snd_wl1 = seq_trimmed ;
        snd_wl2 = ack ;
      }
    else
      cb
      (*: persist timer will be set by [[deliver_out_1]] if this updates the
         window to zero and there is data to send :*)
  in
  let conn' = { conn with control_block } in
  (*: If in [[TIME_WAIT]] or will transition to it from [[CLOSING]], ignore any
     URG, data, or FIN.  Note that in [[FIN_WAIT_1]] or [[FIN_WAIT_2]], we still
     process data, even if [[ourfinisacked]].  :*)
  if conn'.tcp_state = Time_wait || (conn'.tcp_state = Closing && ourfinisacked) then
    the_ststuff now conn' false ourfinisacked, []
  else
    di3_datastuff_really now the_ststuff conn' seg bsd_fast_path ourfinisacked fin

let di3_ststuff id now conn rcvd_fin ourfinisacked =
  let conn' = if rcvd_fin then { conn with cantrcvmore = true } else conn in
  let enter_time_wait =
    let control_block = {
      conn'.control_block with
      tt_2msl = Some (Timers.timer now () (Int64.shift_left Params.tcptv_msl 1)) ;
      tt_rexmt = None ;
      tt_delack = None ;
      tt_conn_est = None ;
      tt_fin_wait_2 = None ;
    } in { conn' with tcp_state = Time_wait ; control_block }
  and state tcp_state = { conn' with tcp_state }
  in
  match conn.tcp_state, rcvd_fin with
  | Established, false -> conn'
  | Established, true -> state Close_wait
  | Close_wait, _ -> conn'
  | Fin_wait_1, false when ourfinisacked ->
    let conn' = state Fin_wait_2 in
    let control_block =
      let tt_fin_wait_2 =
        if conn'.cantrcvmore then
          Some (Timers.timer now () Params.tcptv_maxidle)
        else
          None
      in
      { conn'.control_block with tt_fin_wait_2 }
    in
    { conn' with control_block }
  | Fin_wait_1, false -> conn'
  | Fin_wait_1, true when ourfinisacked -> enter_time_wait
  | Fin_wait_1, true -> state Closing
  | Fin_wait_2, false -> conn'
  | Fin_wait_2, true -> enter_time_wait
  | Closing, _ when ourfinisacked -> enter_time_wait
  | Closing, _ -> conn'
  | Last_ack, false -> conn'
  | Last_ack, true ->
    Log.info (fun m -> m "Last_ack and we received a fin on %a"
                 Connection.pp id);
    assert false
  | Time_wait, _ -> enter_time_wait
  | _ -> assert false

let deliver_in_3 m now id conn seg flag ack =
  m "deliver-in-3";
  (* we expect at most FIN PSH ACK - we drop with reset all other combinations *)
  let* () =
    guard (flag = None || flag = Some `Fin) (`Reset (fun () -> "flags ACK | FIN & ACK"))
  in
  let fin = flag = Some `Fin in
  (* PAWS, timers, rcv_wnd may have opened! updates fin_wait_2 timer *)
  let cb = conn.control_block in
  let wesentafin = Sequence.greater cb.snd_max (Sequence.addi cb.snd_una (Rope.length conn.sndq)) in
  let ourfinisacked = wesentafin && Sequence.greater_equal ack cb.snd_max in
  let control_block = di3_topstuff now conn in
  (* ACK processing *)
  let (conn', outs), cont =
    di3_ackstuff now id { conn with control_block } seg ourfinisacked fin ack
  in
  (* may have some fresh data to report which needs to be acked *)
  Option.fold
    ~none:(Ok (None, []))
    ~some:(fun conn' ->
        let conn'', outs' =
          if cont then
            di3_datastuff now (di3_ststuff id) conn' seg ourfinisacked fin ack
          else
            (conn', [])
        in
        let out = outs @ outs' in
        Ok (Some conn'', out))
    conn'

let deliver_in_7 m id conn seg =
  m "deliver-in-7";
  let cb = conn.control_block in
  if Sequence.equal cb.rcv_nxt seg.Segment.seq then
    (* we rely that dropwithreset does not RST if a RST was received *)
    Error (`Reset (fun () -> "received valid reset"))
  else
    Ok (Segment.make_ack cb ~fin:false id)

let deliver_in_8 m id conn _seg =
  m "deliver-in-8";
  Ok (Segment.make_ack conn.control_block ~fin:false id)

let handle_noconn t now id seg =
  let m = rule t in
  match
    (* TL;DR: if there's a listener, and it is a SYN, we do sth useful. otherwise RST *)
    IS.mem seg.Segment.dst_port t.listeners, seg.Segment.flag = Some `Syn && seg.Segment.ack = None
    (* deliver_in_1 - passive open *)
  with
  | true, true ->
    (* there can't be anything in TIME_WAIT, otherwise we wouldn't end up here *)
    let conn, reply = deliver_in_1 t.mk_notify m t.stats t.rng now id seg in
    { t with connections = CM.add id conn t.connections }, [ reply ]
  | true, false ->
    (* deliver_in_1b *)
    m "deliver-in-1b";
    let out = Option.map (fun _ack -> dropwithreset id seg) seg.Segment.ack in
    t, Option.to_list (Option.join out)
  | false, syn ->
    m "deliver-in-5-6";
    Log.debug (fun m -> m "%a dropping segment with reset (SYN %B) %a"
                  Connection.pp id syn Segment.pp seg);
    (* deliver_in_5 / deliver_in_6 *)
    t, Option.to_list (dropwithreset id seg)

let handle_conn t now id conn seg =
  let m = rule t in
  Log.debug (fun m -> m "%a handle_conn %a@ seg %a" Connection.pp id (pp_conn_state now) conn Segment.pp seg);
  let add conn' =
    Log.debug (fun m -> m "%a now %a" Connection.pp id (pp_conn_state now) conn');
    { t with connections = CM.add id conn' t.connections }
  and drop () =
    Log.debug (fun m -> m "%a dropped" Connection.pp id);
    { t with connections = CM.remove id t.connections }
  in
  let r = match conn.tcp_state with
    | Syn_sent ->
      begin match seg.Segment.ack, seg.Segment.flag with
        | Some ack, Some `Syn ->
          let* c', o = deliver_in_2 m t.stats now id conn seg ack in
          Ok (add c', [ o ])
        | None, Some `Syn ->
          (* simultaneous open: accept anything, send syn+ack *)
          (* let* c', o = deliver_in_2b now id conn seg in *)
          m "deliver_in_2b";
          Ok (drop (), [ ])
        | _, ((None | Some `Rst | Some `Fin) as f) ->
          let* () = deliver_in_2a m conn seg f in
          Ok (drop (), [])
      end
    | Syn_received ->
      (* expected is:
          - ACK with proper seg (3d) ~> ok established
          - stupid ACK 3c -> drop
          - RST (=rcv_nxt) 7a -> zap
          - RST 7e -> drop
          - RST in-window -> challenge-ack
         may hit as well (ignore):
          - FIN [grmbl - just not ack it, will then be handled in established]
          - SYN (simultaneous open..)
         model uses di_3 (there's no separate 3d)

         according to 793, once simultaneous open ends us in syn_received, that
         even may emit syn+ack (with seq = iss) to move forward [but then, as
         well just an ack is possible with seq = iss + 1]
      *)
      let* conn' = deliver_in_3c_3d m t.stats now conn seg in
      Ok (add conn', [])
    | Time_wait when seg.Segment.flag = Some `Syn &&
                     seg.Segment.ack = None &&
                     IS.mem seg.Segment.dst_port t.listeners &&
                     Sequence.less conn.control_block.rcv_nxt seg.Segment.seq ->
      (* RFC1122 4.2.2.13:
            When a connection is closed actively, it MUST linger in
            TIME-WAIT state for a time 2xMSL (Maximum Segment Lifetime).
            However, it MAY accept a new SYN from the remote TCP to
            reopen the connection directly from TIME-WAIT state, if it:

            (1)  assigns its initial sequence number for the new
                 connection to be larger than the largest sequence
                 number it used on the previous connection incarnation,
                 and

            (2)  returns to TIME-WAIT state if the SYN turns out to be
                 an old duplicate.
      *)
      (* model - hostLTSScript:14701 (deliver_in_1):
         If another socket in the [[TIME_WAIT]] state matches the address quad of the SYN segment
         then only proceed with the new incoming connection attempt if the sequence number of the
         segment [[seq]] is strictly greater than the next expected sequence number on the
         [[TIME_WAIT]] socket, [[rcv_nxt]]. This prevents old or duplicate SYN segments from previous
         incarnations of the connection from inadvertently creating new connections.

         Note: this models the behaviour in RFC1122 Section 4.2.2.13 which states that a new [[SYN]]
         with a sequence number larger than the maximum seen in the last incarnation may reopen the
         connection, \ie, reuse the socket for the new connection changing out of the [[TIME_WAIT]]
         state. This is modelled by closing the existing [[TIME_WAIT]] socket and creating the new
         socket from scratch.
      *)
      let conn, reply = deliver_in_1 t.mk_notify m t.stats t.rng now id seg in
      Ok ({ t with connections = CM.add id conn t.connections }, [ reply ])
    | _ ->
      let* () =
        guard (in_window conn.control_block seg)
          (`Drop (fun () -> Fmt.str "in_window seq %a seql %u rcv_nxt %a rcv_wnd %u"
                     Sequence.pp seg.Segment.seq seg.payload_len
                     Sequence.pp conn.control_block.rcv_nxt conn.control_block.rcv_wnd))
      in
      (* RFC5961: challenge acks for SYN and (RST where seq != rcv_nxt), keep state *)
      match seg.Segment.flag, seg.Segment.ack with
      | Some `Rst, _ ->
        let* seg' = deliver_in_7 m id conn seg in
        Ok (t, [ seg' ])
      | Some `Syn, _ ->
        let* seg' = deliver_in_8 m id conn seg in
        Ok (t, [ seg' ])
      | _, None -> Error (`Drop (fun () -> "no ACK"))
      | f, Some ack ->
        let* conn', out = deliver_in_3 m now id conn seg f ack in
        match conn' with
        | None -> Ok (drop (), [])
        | Some conn' ->
          let conn'', out' = match out with
            | [] -> Segment.tcp_output_perhaps now id conn'
            | x -> conn', x
          in
          Ok (add conn'', out')
  in
  match r with
  | Ok (t, a) -> t, a
  | Error (`Drop msg) ->
    Log.debug (fun m -> m "%a dropping segment in %a failed condition %s"
                Connection.pp id pp_fsm conn.tcp_state (msg ()));
    t, []
  | Error (`Reset msg) ->
    Log.debug (fun m -> m "%a reset in %a %s" Connection.pp id pp_fsm conn.tcp_state (msg ()));
    drop (), Option.to_list (dropwithreset id seg)

let handle_segment t now id seg =
  Log.debug (fun m -> m "%a TCP %a" Connection.pp id Segment.pp seg) ;
  let t', out = match CM.find_opt id t.connections with
    | None -> handle_noconn t now id seg
    | Some conn -> handle_conn t now id conn seg
  in
  t', out

let handle_buf t now ~src ~dst data =
  match Segment.decode_and_validate ~src ~dst data with
  | Error (`Msg msg) ->
    Log.debug (fun m -> m "dropping invalid segment %s" msg);
    t, None, []
  | Ok (seg, id) ->
    Tracing.debug (fun m -> m "%a [%a] handle_buf %u %s"
                      Connection.pp id Mtime.pp now
                      seg.payload_len
                      (Base64.encode_string (Cstruct.to_string data)));
    (* deliver_in_3a deliver_in_4 are done now! *)
    let t', outs = handle_segment t now id seg in
    let ev =
      let was_established, was_syn_sent, was_present =
        match CM.find_opt id t.connections with
        | None -> false, false, false
        | Some s -> s.tcp_state = Established, s.tcp_state = Syn_sent, true
      in
      let is_established, is_present, rcv_data, snd_space, rcv_n, snd_n =
        match CM.find_opt id t'.connections with
        | None -> false, false, false, false, None, None
        | Some s ->
          s.tcp_state = Established,
          true,
          Rope.length s.rcvq > 0,
          Rope.length s.sndq < s.sndbufsize,
          Some s.rcv_notify, Some s.snd_notify
      in
      match was_established, is_established, was_present, is_present with
      | false, true, _, _ ->
        (* active open, there's likely someone waiting *)
        let cond = if was_syn_sent then rcv_n else None in
        Some (`Established (id, cond))
      | true, false, _, _
      | _, _, true, false ->
        let opt_cond, conds =
          if rcv_data then
            rcv_n, Option.to_list snd_n
          else
            None, Option.to_list rcv_n @ Option.to_list snd_n
        in
        Some (`Drop (id, opt_cond, conds))
      | _ ->
        match
          (if rcv_data then Option.to_list rcv_n else []) @
          (if snd_space then Option.to_list snd_n else [])
        with
        | [] -> None
        | conds -> Some (`Signal (id, conds))
    in
    List.iter (fun (src', dst', _) ->
        let src, _, dst, _ = id in
        if Ipaddr.compare src' src <> 0 then
          Log.debug (fun m -> m "bad IP reply src' %a vs src %a"
                        Ipaddr.pp src' Ipaddr.pp src);
        if Ipaddr.compare dst' dst <> 0 then
          Log.debug (fun m -> m "bad IP reply dst' %a vs dst %a"
                        Ipaddr.pp dst' Ipaddr.pp dst))
      outs ;
    t', ev, outs
