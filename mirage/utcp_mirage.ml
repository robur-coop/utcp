open Lwt.Infix

let src = Logs.Src.create "tcp.mirage" ~doc:"TCP mirage"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.t) = struct

  let now () = Mtime.of_uint64_ns (Mclock.elapsed_ns ())

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
    mutable tcp : Utcp.state ;
    ip : Ip.t ;
    mutable waiting : (unit, [ `Msg of string ]) result Lwt_condition.t Utcp.FM.t ;
    mutable listeners : (flow -> unit Lwt.t) Port_map.t ;
  }
  and flow = t * Utcp.flow

  let dst (_t, flow) =
    let _, (dst, dst_port) = Utcp.peers flow in
    dst, dst_port

  let close t flow =
    match Utcp.close t.tcp flow with
    | Ok tcp -> t.tcp <- tcp
    | Error `Msg msg -> Log.err (fun m -> m "error in close: %s" msg)

  let rec read (t, flow) =
    match Utcp.recv t.tcp flow with
    | Ok (tcp, data) ->
      t.tcp <- tcp ;
      if Cstruct.length data = 0 then
        let cond = Lwt_condition.create () in
        t.waiting <- Utcp.FM.add flow cond t.waiting;
        Lwt_condition.wait cond >>= fun _ ->
        t.waiting <- Utcp.FM.remove flow t.waiting;
        read (t, flow)
      else
        Lwt.return (Ok (`Data data))
    | Error `Msg msg ->
      close t flow;
      Log.err (fun m -> m "error while read %s" msg);
      (* TODO better error *)
      Lwt.return (Error `Refused)

  let write (t, flow) buf =
    match Utcp.send t.tcp flow buf with
    | Ok tcp -> t.tcp <- tcp ; Lwt.return (Ok ())
    | Error `Msg msg ->
      close t flow;
      Log.err (fun m -> m "error while write %s" msg);
      (* TODO better error *)
      Lwt.return (Error `Refused)

  let writev flow bufs = write flow (Cstruct.concat bufs)

  let close (t, flow) = close t flow ; Lwt.return_unit

  let write_nodelay flow buf = write flow buf

  let writev_nodelay flow bufs = write flow (Cstruct.concat bufs)

  let output_ip t (src, dst, seg) =
    Ip.write t.ip ~src dst `TCP (fun _ -> 0) [seg]

  let create_connection ?keepalive:_ t (dst, dst_port) =
    let src = Ip.src t.ip ~dst in
    let tcp, id, seg = Utcp.connect ~src ~dst ~dst_port t.tcp (now ()) in
    t.tcp <- tcp;
    output_ip t seg >>= function
    | Error e ->
      Log.err (fun m -> m "error sending syn: %a" Ip.pp_error e);
      Lwt.return (Error `Refused)
    | Ok () ->
      let cond = Lwt_condition.create () in
      t.waiting <- Utcp.FM.add id cond t.waiting;
      Lwt_condition.wait cond >|= fun r ->
      t.waiting <- Utcp.FM.remove id t.waiting;
      match r with
      | Ok () -> Ok (t, id)
      | Error `Msg msg ->
        Log.err (fun m -> m "error establishing connection: %s" msg);
        (* TODO better error *)
        Error `Timeout

  let input t ~src ~dst data =
    let tcp, ev, data = Utcp.handle_buf t.tcp (now ()) ~src ~dst data in
    t.tcp <- tcp;
    let find ?f ctx id r =
      match Utcp.FM.find_opt id t.waiting with
      | Some c -> Lwt_condition.signal c r
      | None -> match f with
        | Some f -> f ()
        | None -> Log.warn (fun m -> m "%a not found in waiting (%s)" Utcp.pp_flow id ctx)
    in
    Option.fold ~none:()
      ~some:(function
          | `Established id ->
            let ctx = "established" in
            let f () =
              let (_, port), _ = Utcp.peers id in
              match Port_map.find_opt port t.listeners with
              | None ->
                Log.warn (fun m -> m "%a not found in waiting or listeners (%s)"
                             Utcp.pp_flow id ctx)
              | Some cb ->
                (* NOTE we start an asynchronous task with the callback *)
                Lwt.async (fun () -> cb (t, id))
            in
            find ~f ctx id (Ok ())
          | `Drop id -> find "drop" id (Error (`Msg "dropped"))
          | `Received id -> find "received" id (Ok ()))
      ev;
    (* TODO do not ignore IP write error *)
    let out_ign t s = output_ip t s >|= ignore in
    Lwt_list.iter_p (out_ign t) (Option.to_list data)

  let connect ip =
    let tcp = Utcp.empty R.generate in
    let t = { tcp ; ip ; waiting = Utcp.FM.empty ; listeners = Port_map.empty } in
    Lwt.async (fun () ->
        let timer () =
          let tcp, drops, outs = Utcp.timer t.tcp (now ()) in
          t.tcp <- tcp;
          List.iter (fun id ->
              match Utcp.FM.find_opt id t.waiting with
              | None -> Log.warn (fun m -> m "%a not found in waiting"
                                     Utcp.pp_flow id)
              | Some c ->
                Lwt_condition.signal c (Error (`Msg "timer timed out")))
            drops ;
          (* TODO do not ignore IP write error *)
          let out_ign t s = output_ip t s >|= ignore in
          Lwt_list.iter_p (out_ign t) outs
        and timeout () =
          Time.sleep_ns (Duration.of_ms 100)
        in
        let rec go () =
          Lwt.join [ timer () ; timeout () ] >>=
          go
        in
        go ());
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
