(* (c) 2019 Hannes Mehnert, all rights reserved *)

let src = Logs.Src.create "tcp.timer" ~doc:"TCP timer"
module Log = (val Logs.src_log src : Logs.LOG)

open State

(* hostLTS:17138 *)
let timer_tt_rexmtsyn now shift id conn =
  match conn.tcp_state with
  | Syn_sent (* simultaneous open (deliver_in_2b) may put us into Syn_received *) ->
    if succ shift > Params.tcp_maxrxtshift then begin
      Log.info (fun m -> m "%a syn retransmission reached maxrxtshift, dropping" Connection.pp id);
      let rst = Segment.drop_and_close id conn in
      None, rst
    end else
      let cb = conn.control_block in
      let request_r_scale = if succ shift = 3 then None else cb.request_r_scale in
      let t_rttinf =
        let i = cb.t_rttinf in
        if succ shift > Params.tcp_maxrxtshift / 4 then { i with tf_srtt_valid = false } else i
      in
      let maxseg = Subr.tcp_maxseg conn in
      let control_block = {
        cb with
        tt_rexmt = Some (Timers.timer now (RexmtSyn, succ shift) Params.tcp_syn_backoff.(succ shift)) ;
        t_rttinf = { t_rttinf with t_lastshift = Some (succ shift) ; t_wassyn = true } ;
        request_r_scale ;
        snd_nxt = Sequence.incr cb.iss ;
        snd_recover = Sequence.incr cb.iss ;
        t_rttseg = None ;
        snd_cwnd = maxseg ;
        snd_ssthresh = maxseg * max 2 (min cb.snd_wnd cb.snd_cwnd / (2 * maxseg)) ; (* need to adjust with cc_newreno *)
        t_dupacks = 0 ;
      }
      in
      let conn' = { conn with control_block } in
      Log.info (fun m -> m "%a retransmitting syn %a" Connection.pp id pp_conn_state conn');
      Some conn', Some (Segment.make_syn control_block id)
  | _ ->
    Log.warn (fun m -> m "%a rexmtsyn timer, not in syn_sent state %a"
                Connection.pp id pp_conn_state conn);
    Some conn, None

let timer_tt_rexmt now shift id conn =
  let cb, tcp_state = conn.control_block, conn.tcp_state in
  match tcp_state with
  | Syn_sent | Close_wait | Fin_wait_2 | Time_wait ->
    Log.warn (fun m -> m "%a rexmt timer, in syn_sent, close_wait, fin_wait_2, time_wait state %a"
                 Connection.pp id pp_conn_state conn);
    Some conn, None
  | _ ->
    let maxshift = match tcp_state with Syn_received -> Params.tcp_synackmaxrxtshift | _ -> Params.tcp_maxrxtshift in
    if succ shift > maxshift then begin
      Log.info (fun m -> m "%a retransmission reached maxrxtshift, dropping" Connection.pp id);
      let rst = Segment.drop_and_close id conn in
      None, rst
    end else
      let snd_cwnd_prev, snd_ssthresh_prev = (* , t_badrxtwin *)
        if succ shift = 1 then
          cb.snd_cwnd, cb.snd_ssthresh
        else
          cb.snd_cwnd_prev, cb.snd_ssthresh_prev
      in
      let t_rttinf =
        if succ shift > Params.tcp_maxrxtshift / 4 then
          { cb.t_rttinf with tf_srtt_valid = false }
        else
          cb.t_rttinf
      in
      let control_block = {
        cb with
        tt_rexmt = Some (Timers.timer now (Rexmt, succ shift) Params.tcp_backoff.(succ shift)) ;
        t_rttinf = { t_rttinf with t_lastshift = Some (succ shift) ; t_wassyn = false } ;
        snd_nxt = cb.snd_una ;
        snd_recover = cb.snd_max ;
        t_rttseg = None ;
        snd_cwnd = cb.t_maxseg ;
        snd_ssthresh = cb.t_maxseg ;
        snd_cwnd_prev ; snd_ssthresh_prev ; t_dupacks = 0
      } in
      let conn' = { conn with control_block } in
      let c', out = match tcp_state with
        | Syn_received -> conn', Segment.make_syn_ack control_block id
        | _ -> Segment.tcp_output_really now id false conn'
      in
      Some c', Some out

let timer_tt_persist now shift id conn =
  (* <| tt_rexmt := start_tt_persist (shift+1) tcp_sock.cb.t_rttinf h.arch |> |>) |> in *)
  (* TODO the above takes rto / rttinf into account *)
  if succ shift >= Array.length Params.tcp_backoff then begin
    Log.err (fun m -> m "persist timer shift exceeded backoff array length");
    Some conn, None
  end else
    let tt_rexmt =
      Some (Timers.timer now (Persist, succ shift) Params.tcp_backoff.(succ shift))
    in
    let control_block = { conn.control_block with tt_rexmt } in
    let conn' = { conn with control_block } in
    let conn', seg = Segment.tcp_output_really now id true conn' in
    Some conn', Some seg

let timer_tt_delack now id conn =
  let control_block = { conn.control_block with tt_delack = None } in
  let conn = { conn with control_block } in
  Segment.tcp_output_really now id false conn

let fast_timer t now =
  let connections, out =
    CM.fold (fun id conn (acc, outs) ->
        match conn.control_block.tt_delack with
        | None -> CM.add id conn acc, outs
        | Some timer -> match Timers.timer_expired now timer with
          | None -> CM.add id conn acc, outs
          | Some () ->
            let c', out = timer_tt_delack now id conn in
            CM.add id c' acc, out :: outs)
      t.connections (CM.empty, [])
  in
  { t with connections }, out

let slow_timer t now =
  let connections, outs =
    CM.fold (fun id conn (acc, outs) ->
        let maybe_out = function
          | None -> outs
          | Some out -> out :: outs
        and expired x = match x with
          | None -> None
          | Some timer -> Timers.timer_expired now timer
        and cb = conn.control_block
        in
        let r, out_opt =
          match expired cb.tt_rexmt, expired cb.tt_2msl, expired cb.tt_conn_est, expired cb.tt_fin_wait_2 with
          | Some (RexmtSyn, shift), _, _, _ ->
            Log.warn (fun m -> m "%a syn retransmit expired %a" Connection.pp id pp_conn_state conn);
            if not (conn.tcp_state = Syn_sent) then Log.err (fun m -> m "not in syn_sent");
            timer_tt_rexmtsyn now shift id conn
          | Some (Rexmt, shift), _, _, _ ->
            Log.warn (fun m -> m "%a retransmit expired %a" Connection.pp id pp_conn_state conn);
            timer_tt_rexmt now shift id conn
          | Some (Persist, shift), _, _, _ ->
            Log.warn (fun m -> m "%a persist timer expired %a" Connection.pp id pp_conn_state conn);
            (* it's easy: restart and tcp_output_really! *)
            timer_tt_persist now shift id conn
          | None, Some (), _, _ ->
            (* timer_tt_2msl_1 *)
            Log.warn (fun m -> m "%a 2msl timer expired %a" Connection.pp id pp_conn_state conn);
            if not (conn.tcp_state = Time_wait) then Log.err (fun m -> m "not in time_wait!!!!");
            None, None
          | None, None, Some (), _ ->
            (* timer_tt_conn_est_1 *)
            Log.warn (fun m -> m "%a connection established timer expired %a" Connection.pp id pp_conn_state conn);
            if not (conn.tcp_state = Syn_sent) then Log.err (fun m -> m "not in syn_sent");
            None, Segment.drop_and_close id conn
          | None, None, None, Some () ->
            (* timer_tt_fin_wait_2_1 *)
            Log.warn (fun m -> m "%a fin_wait_2 timer expired %a" Connection.pp id pp_conn_state conn);
            if not (conn.tcp_state = Syn_sent) then Log.err (fun m -> m "not in fin_wait_2");
            None, None
          | None, None, None, None -> Some conn, None
        in
        (match r with None -> acc | Some c -> CM.add id c acc),
        maybe_out out_opt)
      t.connections (CM.empty, [])
  in
  { t with connections }, outs

let ctr = ref 0

(* expected to be called every 100msec - we have slow timers (500ms) and fast timers (200ms) used by delayed ack *)
let timer t now =
  incr ctr ;
  CM.iter (fun id conn ->
      Log.debug (fun m -> m "timer %a is %a" Connection.pp id pp_conn_state conn))
    t.connections ;
  let t, outs =
    if !ctr mod 2 = 0 then fast_timer t now
    else if !ctr mod 5 = 0 then slow_timer t now
    else t, []
  in
  t, List.map (fun (dst, seg) -> dst, Segment.encode_and_checksum ~src:t.ip ~dst seg) outs
