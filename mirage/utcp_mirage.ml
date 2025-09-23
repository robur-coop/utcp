open Lwt.Infix

let src = Logs.Src.create "tcp.mirage" ~doc:"TCP mirage"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.t) = struct

  let now () = Mtime.of_uint64_ns (Mirage_mtime.elapsed_ns ())

  type error = Tcpip.Tcp.error

  let pp_error = Tcpip.Tcp.pp_error

  type write_error = Tcpip.Tcp.write_error

  let pp_write_error = Tcpip.Tcp.pp_write_error

  type ipaddr = Ipaddr.t

  module Port_map = Map.Make (struct
      type t = int
      let compare (a : int) (b : int) = compare a b
    end)

  type t = {
    mutable tcp : (unit, [ `Eof | `Msg of string ]) result Lwt_condition.t Utcp.state ;
    ip : Ip.t ;
    mutable listeners : (flow -> unit Lwt.t) Port_map.t ;
  }
  and flow = t * Utcp.flow

  let dst (_t, flow) =
    let _, (dst, dst_port) = Utcp.peers flow in
    dst, dst_port

  let src (_t, flow) =
    let (src, src_port), _ = Utcp.peers flow in
    src, src_port

  let output_ip t (src, dst, seg) =
    let size = Utcp.Segment.length seg in
    Log.debug (fun m -> m "output to %a: %a" Ipaddr.pp dst Utcp.Segment.pp seg);
    Ip.write t.ip ~src dst `TCP ~size
      (fun buf ->
         Utcp.Segment.encode_and_checksum_into (now ()) buf ~src ~dst seg;
         size) []

  let output_ign t segs =
    List.fold_left (fun r seg ->
        r >>= fun () ->
        output_ip t seg >|= function
        | Error e ->
          let _, dst, _ = seg in
          Log.err (fun m -> m "error sending data to %a: %a" Ipaddr.pp dst Ip.pp_error e)
        | Ok () -> ())
      Lwt.return_unit segs

  let read (t, flow) =
    match Utcp.recv t.tcp (now ()) flow with
    | Ok (tcp, [], cond, segs) -> (
      t.tcp <- tcp ;
      output_ign t segs >>= fun () ->
      Lwt_condition.wait cond >>= fun r ->
      match r with
      | Error `Eof ->
        Lwt.return (Ok `Eof)
      | Error `Msg msg ->
        Log.err (fun m -> m "%a error %s from condition while recv" Utcp.pp_flow flow msg);
        (* TODO better error *)
        Lwt.return (Error `Refused)
      | Ok () ->
        match Utcp.recv t.tcp (now ()) flow with
        | Ok (tcp, data, _cond, segs) ->
          t.tcp <- tcp ;
          output_ign t segs >>= fun () ->
          begin match data with
          | [] -> Lwt.return (Ok `Eof)
          | sstr ->
              let cs = Cstruct.of_string (String.concat "" sstr) in
              Lwt.return (Ok (`Data cs)) end
        | Error `Eof ->
          Lwt.return (Ok `Eof)
        | Error `Msg msg ->
          Log.err (fun m -> m "%a error while read (second recv) %s" Utcp.pp_flow flow msg);
          (* TODO better error *)
          Lwt.return (Error `Refused)
        | Error `Not_found -> Lwt.return (Error `Refused))
    | Ok (tcp, sstr, _cond, segs) ->
      t.tcp <- tcp ;
      output_ign t segs >>= fun () ->
      let cs = Cstruct.of_string (String.concat "" sstr) in
      Lwt.return (Ok (`Data cs))
    | Error `Eof ->
      Lwt.return (Ok `Eof)
    | Error `Msg msg ->
      Log.err (fun m -> m "%a error while read %s" Utcp.pp_flow flow msg);
      (* TODO better error *)
      Lwt.return (Error `Refused)
    | Error `Not_found -> Lwt.return (Error `Refused)

  let rec write (t, flow) buf =
    match Utcp.send t.tcp (now ()) flow buf with
    | Ok (tcp, bytes_sent, cond, segs) ->
      t.tcp <- tcp ;
      output_ign t segs >>= fun () ->
      if bytes_sent < String.length buf then
        (* partial write *)
        Lwt_condition.wait cond >>= fun r ->
        match r with
        | Error `Eof ->
          Lwt.return (Error `Closed)
        | Error `Msg msg ->
          Log.err (fun m -> m "%a error %s from condition while sending" Utcp.pp_flow flow msg);
          Lwt.return (Error `Closed)
        | Ok () ->
          let buf = String.sub buf bytes_sent (String.length buf - bytes_sent) in
          write (t, flow) buf
      else
        Lwt.return (Ok ())
    | Error `Msg msg ->
      Log.err (fun m -> m "%a error while write %s" Utcp.pp_flow flow msg);
      Lwt.return (Error `Closed)
    | Error `Not_found -> Lwt.return (Error `Refused)

  let writev flow bufs = write flow (Cstruct.to_string (Cstruct.concat bufs))
  let write flow buf = write flow (Cstruct.to_string buf)

  let close (t, flow) =
    match Utcp.close t.tcp (now ()) flow with
    | Ok (tcp, segs) ->
      t.tcp <- tcp ;
      output_ign t segs
    | Error `Msg msg ->
      Log.err (fun m -> m "%a error in close: %s" Utcp.pp_flow flow msg);
      Lwt.return_unit
    | Error `Not_found -> Lwt.return_unit

  let shutdown (t, flow) mode =
    match Utcp.shutdown t.tcp (now ()) flow mode with
    | Ok (tcp, segs) ->
      t.tcp <- tcp ;
      output_ign t segs
    | Error `Msg msg ->
      Log.err (fun m -> m "%a error in shutdown: %s" Utcp.pp_flow flow msg);
      Lwt.return_unit
    | Error `Not_found -> Lwt.return_unit

  let write_nodelay flow buf = write flow buf

  let writev_nodelay flow bufs = write flow (Cstruct.concat bufs)

  let create_connection ?keepalive:_ t (dst, dst_port) =
    let src = Ip.src t.ip ~dst in
    let tcp, id, cond, seg = Utcp.connect ~src ~dst ~dst_port t.tcp (now ()) in
    t.tcp <- tcp;
    output_ip t seg >>= function
    | Error e ->
      Log.err (fun m -> m "%a error sending syn: %a" Utcp.pp_flow id Ip.pp_error e);
      Lwt.return (Error `Refused)
    | Ok () ->
      Lwt_condition.wait cond >|= fun r ->
      match r with
      | Ok () -> Ok (t, id)
      | Error `Eof ->
        Log.err (fun m -> m "%a error establishing connection (timeout)" Utcp.pp_flow id);
        (* TODO better error *)
        Error `Timeout
      | Error `Msg msg ->
        Log.err (fun m -> m "%a error establishing connection: %s" Utcp.pp_flow id msg);
        (* TODO better error *)
        Error `Timeout

  let input t ~src ~dst data =
    let tcp, ev, segs = Utcp.handle_buf t.tcp (now ()) ~src ~dst data in
    t.tcp <- tcp;
    Option.fold ~none:()
      ~some:(function
          | `Established (id, cond) ->
            (match cond with
             | None ->
               let (_, port), _ = Utcp.peers id in
               (match Port_map.find_opt port t.listeners with
                | None ->
                  Log.debug (fun m -> m "%a not found in waiting or listeners"
                                Utcp.pp_flow id)
                | Some cb ->
                  (* NOTE we start an asynchronous task with the callback *)
                  Lwt.async (fun () -> cb (t, id)))
             | Some cond ->
               Lwt_condition.signal cond (Ok ()))
          | `Drop (_id, c_opt, cs) ->
            List.iter (fun c -> Lwt_condition.signal c (Error `Eof)) cs;
            Option.iter (fun c -> Lwt_condition.signal c (Ok ())) c_opt
          | `Signal (_id, conds) ->
            List.iter (fun c -> Lwt_condition.signal c (Ok ())) conds
        )
      ev;
    (* TODO do not ignore IP write error *)
    output_ign t segs

  let connect id ip =
    Log.info (fun m -> m "starting µTCP on %S" id);
    let tcp = Utcp.empty Lwt_condition.create id Mirage_crypto_rng.generate in
    let t = { tcp ; ip ; listeners = Port_map.empty } in
    Lwt.async (fun () ->
        let rec timer n =
          let tcp, drops, outs = Utcp.timer t.tcp (now ()) in
          t.tcp <- tcp;
          List.iter (fun (_id, err, rcv, snd) ->
              let err = Error (match err with
                  | `Retransmission_exceeded -> `Msg "retransmission exceeded"
                  | `Timer_2msl -> `Eof
                  | `Timer_connection_established -> `Eof
                  | `Timer_fin_wait_2 -> `Eof)
              in
              Lwt_condition.signal rcv err;
              Lwt_condition.signal snd err;
            )
            drops;
          (* TODO do not ignore IP write error *)
          Lwt_list.iter_p (fun data -> output_ip t data >|= ignore) outs >>= fun () ->
          Mirage_sleep.ns (Duration.of_ms 100) >>= fun () ->
          (timer [@tailcall]) (succ n)
        in
        timer 0);
    t

  let listen t ~port ?keepalive:_ callback =
    let tcp = Utcp.start_listen t.tcp port in
    t.tcp <- tcp;
    t.listeners <- Port_map.add port callback t.listeners

  let unlisten t ~port =
    let tcp = Utcp.stop_listen t.tcp port in
    t.tcp <- tcp;
    t.listeners <- Port_map.remove port t.listeners

  let disconnect _t =
    Lwt.return_unit
end
