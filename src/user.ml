(* (c) 2019 Hannes Mehnert, all rights reserved *)
open State

let guard p e = if p then Ok () else Error e

let ( let* ) = Result.bind

let src = Logs.Src.create "tcp.user" ~doc:"TCP user"
module Log = (val Logs.src_log src : Logs.LOG)

let connect ~src ?src_port ~dst ~dst_port t now =
  let src_port = match src_port with
    | None -> Randomconv.int16 t.rng
    | Some p -> p
  in
  let id = src, src_port, dst, dst_port in
  let conn =
    let iss = Sequence.of_int32 (Randomconv.int32 t.rng) in
    let rcv_wnd = Params.so_rcvbuf in
    let advmss = Subr.tcp_mssopt id in
    let t_rttseg = Some (now, iss) in
    let control_block = {
      initial_cb with
      tt_rexmt = Subr.start_tt_rexmt_syn now 0 false initial_cb.t_rttinf ;
      tt_conn_est = Some (Timers.timer now () Params.tcptv_keep_init) ;
      snd_una = iss ;
      snd_nxt = Sequence.incr iss ;
      snd_max = Sequence.incr iss ;
      iss ;
      rcv_wnd ;
      request_r_scale = Some Params.scale ;
      rcv_adv = Sequence.of_int32 (Int32.of_int rcv_wnd) ; (* rcv_nxt is 0 anyways, this is void *)
      tf_rxwin0sent = (rcv_wnd = 0);
      t_advmss = advmss ;
      t_rttseg
    } in
    conn_state ~rcvbufsize:rcv_wnd ~sndbufsize:Params.so_sndbuf Syn_sent control_block
  in
  let _, _, seg = Segment.make_syn conn.control_block id in
  let data = Segment.encode_and_checksum ~src ~dst seg in
  let connections =
    Log.debug (fun m -> m "%a active open %a" Connection.pp id pp_conn_state conn);
    CM.add id conn t.connections
  in
  { t with connections }, id, (src, dst, data)

(* it occurs that all these functions below are not well suited for sending out
   segments, a tcp_output(_really) will for sure help *)

(* or should only a timer be responsible for outputting data? sounds a bit weird *)

(* in real, this is shutdown `readwrite (close_2) - and we do this in any state *)
let close t id =
  match CM.find_opt id t.connections with
  | None -> Error (`Msg "no connection")
  | Some conn ->
    let* () =
      guard (behind_established conn.tcp_state) (`Msg "not yet established")
    in
    let control_block = { conn.control_block with tf_shouldacknow = true } in
    let conn' =
      let cantsndmore = true and cantrcvmore = true and rcvq = Cstruct.empty in
      { conn with control_block; cantsndmore; cantrcvmore; rcvq }
    in
    Ok { t with connections = CM.add id conn' t.connections }

let send t id buf =
  match CM.find_opt id t.connections with
  | None -> Error (`Msg "no connection")
  | Some conn ->
    let* () =
      guard (behind_established conn.tcp_state) (`Msg "not yet established")
    in
    let* () =
      guard (not conn.cantsndmore) (`Msg "cant write")
    in
    let sndq = Cstruct.append conn.sndq buf in
    let conn' = { conn with sndq } in
    Ok { t with connections = CM.add id conn' t.connections }

let recv t id =
  match CM.find_opt id t.connections with
  | None -> Error (`Msg "no connection")
  | Some conn ->
    let* () =
      guard (behind_established conn.tcp_state) (`Msg "not yet connected")
    in
    let* () = guard (not conn.cantrcvmore) (`Msg "cant recv, EOF") in
    let rcvq = conn.rcvq in
    let conn' = { conn with rcvq = Cstruct.empty } in
    Ok ({ t with connections = CM.add id conn' t.connections }, rcvq)
