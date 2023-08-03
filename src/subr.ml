(* (c) 2019 Hannes Mehnert, all rights reserved *)
open State

let src = Logs.Src.create "tcp.subr" ~doc:"TCP subr"
module Log = (val Logs.src_log src : Logs.LOG)

(* tcp_input.c:3736 *)
let tcp_mssopt _conn =
  let mss = Params.mssdflt
  and maxmtu = 1500 (* tcp_maxmtu - lookup the routing entry of this connection *)
  and min_protoh = 40
  and thcmtu = 0 (* tcp_hc_getmtu (from hostcache) *)
  in
  if maxmtu > 0 && thcmtu > 0 then
    (min maxmtu thcmtu) - min_protoh
  else if maxmtu > 0 || thcmtu > 0 then
    (max maxmtu thcmtu) - min_protoh
  else
    mss

(* tcp_subr.c:2908 *)
let tcp_maxseg conn =
  let optlen = match conn.tcp_state with
    | Syn_received | Syn_sent ->
      let maxseg_length = 4
      and window_sc_length =
        match conn.control_block.request_r_scale with None -> 0 | Some _ -> 3 + 1
      in
      maxseg_length + window_sc_length
      (* if (tp->t_flags & TF_SACK_PERMIT)
       *     optlen += PAD(TCPOLEN_SACK_PERMITTED); *)
    | _ -> 0
    (* if ((tp->t_flags & TF_SACK_PERMIT) && tp->rcv_numsacks > 0) {
     * 	optlen += TCPOLEN_SACKHDR;
     * 	optlen += tp->rcv_numsacks * TCPOLEN_SACK;
     * 	optlen = PAD(optlen);
     * } *)
  in
  conn.control_block.t_maxseg - optlen

(* utils:85 *)
let roundup bs v = (v + (pred bs) / bs) * bs

(* auxFns:386 tcp_input:332 *)
let calculate_buf_sizes (* conn *) cb_t_maxseg seg_mss bw_delay_product_for_rt rcvbufsize sndbufsize =
  (* BSD let t_maxseg = tcp_maxseg conn in *)
  let t_maxseg' =
    (*: TCPv2p901 claims min 32 for "sanity"; FreeBSD4.6 has 64 in |tcp_mss()|.
        BSD has the route MTU if avail, or [[MIN MSSDFLT (link MTU)]] otherwise, as the first argument
        of the MIN below.  That is the same calculation as we did in [[connect_1]]. We don't repeat it,
        but use the cached value in [[cb.t_maxseg]]. :*)
    min cb_t_maxseg (max 64 (match seg_mss with None -> Params.mssdflt | Some x -> x))
  in
  let rcvbufsize' = match bw_delay_product_for_rt with None -> rcvbufsize | Some x -> x in
  let rcvbufsize'',t_maxseg'' =
    if rcvbufsize' < t_maxseg' then
      rcvbufsize', rcvbufsize'
    else
      min Params.sb_max (roundup t_maxseg' rcvbufsize'), t_maxseg'
  in
  (* buffootle: snd *)
  let sndbufsize' = match bw_delay_product_for_rt with None -> sndbufsize | Some x -> x in
  let sndbufsize'' =
    if sndbufsize' < t_maxseg'' then
      sndbufsize'
    else
      min Params.sb_max (roundup t_maxseg' sndbufsize')
  in
  (* compute initial cwnd *)
  let snd_cwnd = min (4 * t_maxseg'') (max (2 * t_maxseg'') 4380) in
  rcvbufsize'', sndbufsize'', t_maxseg'', snd_cwnd

let calculate_bsd_rcv_wnd conn =
  max (Sequence.window conn.control_block.rcv_adv conn.control_block.rcv_nxt)
    (conn.rcvbufsize - Cstruct.length conn.rcvq)

let update_rtt rtt ri =
  let rtt = Mtime.Span.to_uint64_ns rtt in
  let t_srtt', t_rttvar' =
    if ri.tf_srtt_valid then
      let delta     = Int64.(sub (sub rtt (Duration.of_ms 1)) ri.t_srtt) in
      let vardelta  = Int64.(sub (abs delta) ri.t_rttvar) in
      let t_srtt'   = max (Duration.of_ms 16) Int64.(add ri.t_srtt (shift_right delta 3))
      and t_rttvar' = max (Duration.of_ms 32) Int64.(add ri.t_rttvar (shift_right vardelta 2))
      (* BSD behaviour is never to let these go to zero, but clip at the least
         positive value.  Since SRTT is measured in 1/32 tick and RTTVAR in
         1/16 tick, these are the minimum values.  A more natural implementation
         would clip these to zero. *)
      in
      t_srtt', t_rttvar'
    else
      let t_srtt' = rtt
      and t_rttvar' = Int64.shift_right rtt 1
      in
      t_srtt', t_rttvar'
  in
  { ri with
    t_rttupdated = ri.t_rttupdated + 1;
    tf_srtt_valid = true;
    t_srtt = t_srtt';
    t_rttvar = t_rttvar';
    t_lastrtt = Some rtt;
    t_lastshift = Some 0;
    t_wassyn = false  (* if t_lastshift=0, this doesn't make a difference *)
    (* t_softerror, t_rttseg, and t_rxtcur must be handled by the caller *)
  }

(* auxFns:864 *)
let expand_cwnd ssthresh maxseg maxwin cwnd =
  min maxwin (cwnd + (if cwnd > ssthresh then max 1 ((maxseg * maxseg) / cwnd) else maxseg))

(* auxFns:657 *)
let computed_rto backoffs shift ri =
  Int64.(mul backoffs.(shift)
           (max ri.t_rttmin Int64.(add ri.t_srtt (shift_left ri.t_rttvar 2))))

(* auxFns:663 *)
let computed_rxtcur ri =
  max ri.t_rttmin
    (min Params.tcptv_rexmtmax
       (computed_rto
          (if ri.t_wassyn then Params.tcp_syn_backoff else Params.tcp_backoff)
          (match ri.t_lastshift with None -> 0 | Some x -> x) ri))

(* auxFns:692 *)
let start_tt_rexmt_gen mode backoffs now shift wantmin ri =
  let rxtcur =
    max (if wantmin then
           max ri.t_rttmin
             (match ri.t_lastrtt with None -> 0L | Some v -> Int64.add v (Duration.of_ms 2))
         else ri.t_rttmin)
      (min Params.tcptv_rexmtmax (* better not be infinite! *)
         (computed_rto backoffs shift ri))
  in
  Log.debug (fun m -> m "starting rexmt timer %a (backoff is %a)"
                Duration.pp rxtcur Duration.pp backoffs.(shift));
  Some (Timers.timer now (mode, shift) rxtcur)

let start_tt_rexmt_syn = start_tt_rexmt_gen RexmtSyn Params.tcp_syn_backoff

let start_tt_rexmt = start_tt_rexmt_gen Rexmt Params.tcp_backoff

let start_tt_persist now shift ri =
  let cur = max Params.tcptv_persmin (* better not be infinite! *)
      (min Params.tcptv_persmax (* better not be infinite! *)
         (computed_rto Params.tcp_backoff shift ri))
  in
  Log.debug (fun m -> m "starting persist timer %a (backoff is %a)"
                Duration.pp cur Duration.pp Params.tcp_backoff.(shift));
  Some (Timers.timer now (Persist, shift) cur)
