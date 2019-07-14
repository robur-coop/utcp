(* (c) 2019 Hannes Mehnert, all rights reserved *)

let src = Logs.Src.create "tcp.timer" ~doc:"TCP timer"
module Log = (val Logs.src_log src : Logs.LOG)

open State

(* hostLTS:17138 *)
let timer_tt_rexmtsyn now shift id conn =
  match conn.tcp_state with
  | Syn_sent (* simultaneous open (deliver_in_2b) may put us into Syn_received *) ->
    if shift + 1 > Params.tcp_maxrxtshift then begin
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
      Logs.info (fun m -> m "%a retransmitting syn %a" Connection.pp id pp_conn_state conn');
      Some conn', Some (Segment.make_syn control_block id)
  | _ -> assert false

let ctr = ref 0

(* expected to be called every 100msec - we have slow timers (500ms) and fast timers (200ms) used by delayed ack *)
let timer t now =
  incr ctr ;
  CM.iter (fun id conn ->
      Log.debug (fun m -> m "timer %a is %a" Connection.pp id pp_conn_state conn))
    t.connections ;
  let t, outs =
    if !ctr mod 2 = 0 then
      (* fast timer *)
      t, []
    else if !ctr mod 5 = 0 then
      (* slow timers *)
      let connections, outs =
        CM.fold (fun id conn (acc, outs) ->
            let maybe_out = function
              | None -> outs
              | Some out ->
                let _, _, dst, _ = id in
                (dst, out) :: outs
            in
            match conn.control_block.tt_rexmt with
            | None -> CM.add id conn acc, outs
            | Some timer ->
              match Timers.timer_expired now timer with
              | None -> CM.add id conn acc, outs
              | Some (RexmtSyn, shift) ->
                begin match timer_tt_rexmtsyn now shift id conn with
                  | None, out -> acc, maybe_out out
                  | Some c', out -> CM.add id c' acc, maybe_out out
                end
              | _ -> CM.add id conn acc, outs)
          t.connections (CM.empty, [])
      in
      { t with connections }, outs
    else (* nothing to do *)
      t, []
  in
  t, List.map (fun (dst, seg) -> dst, Segment.encode_and_checksum ~src:t.ip ~dst seg) outs
