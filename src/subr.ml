(* (c) 2019 Hannes Mehnert, all rights reserved *)
open State

(* tcp_input.c:3736 *)
let tcp_mssopt _conn =
  let mss = Params.mssdflt
  and maxmtu = (* tcp_maxmtu - lookup the routing entry of this connection *) 0
  and min_protoh = 40
  and thcmtu = (* tcp_hc_getmtu (from hostcache) *) 0
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
      and window_sc_length = 3 + 1
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
      min Params.sb_max (roundup t_maxseg' rcvbufsize'),
      t_maxseg'
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
