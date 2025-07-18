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
  Tracing.debug (fun m -> m "%a [%a] connect" Connection.pp id Mtime.pp now);
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
    conn_state now t.mk_notify ~rcvbufsize:rcv_wnd ~sndbufsize:Params.so_sndbuf Syn_sent control_block
  in
  let _, _, seg = Segment.make_syn conn.control_block id in
  let connections =
    Log.debug (fun m -> m "%a active open %a" Connection.pp id (pp_conn_state now) conn);
    CM.add id conn t.connections
  in
  Stats.incr_active t.stats;
  { t with connections }, id, conn.rcv_notify, (src, dst, seg)

(* shutdown_1 and shutdown_3 *)
let shutdown t now id v =
  Tracing.debug (fun m ->
      let side = match v with
        | `read -> "read"
        | `write -> "write"
        | `read_write -> "readwrite"
      in
      m "%a [%a] shutdown_%s" Connection.pp id Mtime.pp now side);
  match CM.find_opt id t.connections with
  | None -> Error `Not_found
  | Some conn ->
    if conn.tcp_state = Established then
      let write = match v with `write | `read_write -> true | `read -> false
      and read = match v with `read | `read_write -> true | `write -> false
      in
      let cantsndmore = write || conn.cantsndmore
      and cantrcvmore = read || conn.cantrcvmore
      in
      let rcvq = if read then Rope.empty else conn.rcvq in
      let conn' =
        { conn with cantsndmore; cantrcvmore; rcvq }
      in
      let conn', out =
        (* if only shutdown read side, or we already closed something *)
        if conn.cantsndmore || v = `read then
          conn', []
        else
          Segment.tcp_output_perhaps now id conn'
      in
      Ok ({ t with connections = CM.add id conn' t.connections }, out)
    else
      Error (`Msg "not connected")

(* in real, this is shutdown `readwrite (close_2) - and we do this in any state *)
(* there's as well close_3 (the abortive close, i.e. send a RST) -- done when SO_LINGER = 0 *)
let close t now id =
  Tracing.debug (fun m -> m "%a [%a] close" Connection.pp id Mtime.pp now);
  match CM.find_opt id t.connections with
  | None -> Error `Not_found
  | Some conn ->
    (* see above, should deal with all states of conn *)
    let* () =
      guard (behind_established conn.tcp_state) (`Msg "not yet established")
    in
    let conn' =
      let cantsndmore = true and cantrcvmore = true and rcvq = Rope.empty in
      { conn with cantsndmore; cantrcvmore; rcvq }
    in
    (* if we've already been close()d, don't need to output anything *)
    let conn', out =
      if conn.cantsndmore then
        conn', []
      else
        Segment.tcp_output_perhaps now id conn'
    in
    Ok ({ t with connections = CM.add id conn' t.connections }, out)

let send t now id ?(off= 0) ?len buf =
  Tracing.debug (fun m -> m "%a [%a] send %u %s" Connection.pp id Mtime.pp now
                   (String.length buf)
                   (Base64.encode_string buf));
  match CM.find_opt id t.connections with
  | None -> Error `Not_found
  | Some conn ->
    let* () =
      guard (behind_established conn.tcp_state) (`Msg "not yet established")
    in
    let* () =
      guard (not conn.cantsndmore) (`Msg "cant write")
    in
    let len = match len with
      | Some len -> len
      | None -> String.length buf - off in
    let space = max 0 (conn.sndbufsize - Rope.length conn.sndq) in
    let len = if space < len then space else len in
    let sndq = Rope.append conn.sndq ~off ~len buf in
    let conn' = { conn with sndq } in
    let conn', out = Segment.tcp_output_perhaps now id conn' in
    Ok ({ t with connections = CM.add id conn' t.connections }, len, conn'.snd_notify, out)

let recv t now id =
  Tracing.debug (fun m -> m "%a [%a] receive" Connection.pp id Mtime.pp now);
  match CM.find_opt id t.connections with
  | None -> Error `Not_found
  | Some conn ->
    let* () =
      guard (behind_established conn.tcp_state) (`Msg "not yet connected")
    in
    let rcvq = Rope.to_strings conn.rcvq in
    let* () = guard (not (Rope.length conn.rcvq = 0 && conn.cantrcvmore)) `Eof in
    let conn' = { conn with rcvq = Rope.empty } in
    let conn', out = Segment.tcp_output_perhaps now id conn' in
    Ok ({ t with connections = CM.add id conn' t.connections }, rcvq, conn'.rcv_notify, out)
