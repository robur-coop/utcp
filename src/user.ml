(* (c) 2019 Hannes Mehnert, all rights reserved *)

open State

open Rresult.R.Infix

let src = Logs.Src.create "tcp.user" ~doc:"TCP user"
module Log = (val Logs.src_log src : Logs.LOG)

let connect t now ?src_port dst dst_port =
  let src_port = match src_port with
    | None -> Randomconv.int16 t.rng
    | Some p -> p
  in
  let conn =
    let iss = Sequence.of_int32 (Randomconv.int32 t.rng) in
    let rcv_wnd = Params.so_rcvbuf in
    let advmss = None in
    let request_r_scale = None (* TODO *) in
    let t_rttseg = Some (now, iss) in
    let control_block = {
      initial_cb with
      tt_rexmt = Some (Timers.timer now (RexmtSyn, 0) Params.tcp_syn_backoff.(0)) ;
      tt_conn_est = Some (Timers.timer now () Params.tcptv_keep_init) ;
      snd_una = iss ;
      snd_nxt = Sequence.incr iss ;
      snd_max = Sequence.incr iss ;
      iss ;
      rcv_wnd ;
      rcv_adv = Sequence.of_int32 (Int32.of_int rcv_wnd) ; (* rcv_nxt is 0 anyways, this is void *)
      tf_rxwin0sent = (rcv_wnd = 0);
      t_advmss = advmss ;
      request_r_scale ;
      t_rttseg
    } in
    conn_state ~rcvbufsize:rcv_wnd ~sndbufsize:Params.so_sndbuf Syn_sent control_block
  in
  let seg = Segment.make_syn conn.control_block ~src_port ~dst_port in
  let data = Segment.encode_and_checksum ~src:t.ip ~dst seg in
  let id = t.ip, src_port, dst, dst_port in
  let connections =
    Log.debug (fun m -> m "%a active open %a" Connection.pp id pp_conn_state conn);
    CM.add id conn t.connections
  in
  { t with connections }, id, data

(* it occurs that all these functions below are not well suited for sending out
   segments, a tcp_output(_really) will for sure help *)

(* or should only a timer be responsible for outputting data? sounds a bit weird *)

(* in real, this is shutdown `write *)
let close t id =
  match CM.find_opt id t.connections with
  | None -> Error (`Msg "no connection")
  | Some conn ->
    (match conn.tcp_state with
     | Close_wait -> Ok Last_ack
     | Established -> Ok Fin_wait_1
     | _ -> Error (`Msg "wrong state")) >>| fun tcp_state ->
    let conn' = { conn with tcp_state ; cantsndmore = true } in
    Log.debug (fun m -> m "%a close %a" Connection.pp id pp_conn_state conn');
    let _, src_port, dst, dst_port = quad t id in
    (* that's a bit ad-hoc since there may still be data in the outq *)
    let seg = Segment.make_fin_ack conn.control_block ~src_port ~dst_port in
    let data = Segment.encode_and_checksum ~src:t.ip ~dst seg in
    { t with connections = CM.add id conn' t.connections }, (dst, data)

(* this is usually known as close / tcp_close *)
let reset t id = match CM.find_opt id t.connections with
  | None -> Error (`Msg "no connection")
  | Some c ->
    let _, src_port, dst, dst_port = quad t id in
    let rst = Segment.make_reset c.control_block ~src_port ~dst_port in
    let data = Segment.encode_and_checksum ~src:t.ip ~dst rst in
    Log.debug (fun m -> m "%a drop %a reset %a"
                  Connection.pp id pp_conn_state c Segment.pp rst);
    Ok ({ t with connections = CM.remove id t.connections }, (dst, data))

let send _t _id _buf =
  assert false

let recv _t _id =
  assert false
