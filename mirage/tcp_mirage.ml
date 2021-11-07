open Lwt.Infix

let src = Logs.Src.create "tcp.mirage" ~doc:"TCP mirage"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Mirage_protocols.IPV4) = struct

  let now () = Mtime.of_uint64_ns (Mclock.elapsed_ns ())

  type error = Mirage_protocols.Tcp.error

  let pp_error = Mirage_protocols.Tcp.pp_error

  type write_error = Mirage_protocols.Tcp.write_error

  let pp_write_error = Mirage_protocols.Tcp.pp_write_error

  type ipaddr = Ip.ipaddr

  type ipinput = src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit Lwt.t

  type t = {
    mutable tcp : Tcp.State.t ;
    ip : Ip.t ;
    mutable waiting : (unit, [ `Msg of string ]) result Lwt_condition.t Tcp.State.CM.t ;
  }

  type flow = t * Tcp.State.Connection.t

  type listener = {
    process : flow -> unit Lwt.t;
    keepalive : Mirage_protocols.Keepalive.t option;
  }

  (* the quad src and dst depends on client and server connections *)
  let dst (t, (src, src_port, dst, dst_port)) =
    let src = match src with Ipaddr.V4 ip -> ip | _ -> assert false
    and dst = match dst with Ipaddr.V4 ip -> ip | _ -> assert false
    in
    if List.mem src (Ip.get_ip t.ip) then
      dst, dst_port
    else
      src, src_port

  let rec read (t, flow) =
    match Tcp.User.recv t.tcp flow with
    | Ok (tcp, data) ->
      t.tcp <- tcp ;
      if Cstruct.length data = 0 then
        let cond = Lwt_condition.create () in
        t.waiting <- Tcp.State.CM.add flow cond t.waiting;
        Lwt_condition.wait cond >>= fun _ ->
        t.waiting <- Tcp.State.CM.remove flow t.waiting;
        read (t, flow)
      else
        Lwt.return (Ok (`Data data))
    | Error `Msg msg ->
      Log.err (fun m -> m "error while read %s" msg);
      (* TODO better error *)
      Lwt.return (Error `Refused)

  let write (t, flow) buf =
    match Tcp.User.send t.tcp flow buf with
    | Ok tcp -> t.tcp <- tcp ; Lwt.return (Ok ())
    | Error `Msg msg ->
      Log.err (fun m -> m "error while write %s" msg);
      (* TODO better error *)
      Lwt.return (Error `Refused)

  let writev flow bufs = write flow (Cstruct.concat bufs)

  let close (t, flow) =
    match Tcp.User.close t.tcp flow with
    | Ok tcp -> t.tcp <- tcp ; Lwt.return_unit
    | Error `Msg msg ->
      Log.err (fun m -> m "error while close %s" msg);
      Lwt.return_unit

  let write_nodelay flow buf = write flow buf

  let writev_nodelay flow bufs = write flow (Cstruct.concat bufs)

  let output_ip t (src, dst, seg) =
    match src, dst with
    | Ipaddr.V4 src, Ipaddr.V4 dst ->
      Ip.write t.ip ~src dst `TCP (fun _ -> 0) [seg]
    | _ -> invalid_arg "bad IP version from timer"

  let create_connection ?keepalive:_ t (dst, dst_port) =
    let src = Ipaddr.V4 (Ip.src t.ip ~dst) and dst = Ipaddr.V4 dst in
    let tcp, id, seg = Tcp.User.connect ~src ~dst ~dst_port t.tcp (now ()) in
    t.tcp <- tcp;
    output_ip t seg >>= function
    | Error e ->
      Log.err (fun m -> m "error sending syn: %a" Ip.pp_error e);
      Lwt.return (Error `Refused)
    | Ok () ->
      let cond = Lwt_condition.create () in
      t.waiting <- Tcp.State.CM.add id cond t.waiting;
      Lwt_condition.wait cond >|= fun r ->
      t.waiting <- Tcp.State.CM.remove id t.waiting;
      match r with
      | Ok () -> Ok (t, id)
      | Error `Msg msg ->
        Log.err (fun m -> m "error establishing connection: %s" msg);
        (* TODO better error *)
        Error `Timeout

  (* TODO something with listeners! *)
  let input t ~listeners:_ ~src ~dst data =
    let src = Ipaddr.V4 src and dst = Ipaddr.V4 dst in
    let tcp, ev, data = Tcp.Input.handle_buf t.tcp (now ()) ~src ~dst data in
    t.tcp <- tcp;
    let find ctx id r =
      match Tcp.State.CM.find_opt id t.waiting with
      | None ->
        Log.warn (fun m -> m "%a not found in waiting (%s)"
                     Tcp.State.Connection.pp id ctx)
      | Some c -> Lwt_condition.broadcast c r
    in
    Option.fold ~none:()
      ~some:(function
          | `Established id -> find "established" id (Ok ())
          | `Drop id -> find "drop" id (Error (`Msg "dropped"))
          | `Received id -> find "received" id (Ok ()))
      ev;
    (* TODO do not ignore IP write error *)
    let out_ign t s = output_ip t s >|= ignore in
    Lwt_list.iter_p (out_ign t) (Option.to_list data)

  let connect ip =
    let tcp = Tcp.State.empty R.generate in
    let t = { tcp ; ip ; waiting = Tcp.State.CM.empty } in
    Lwt.async (fun () ->
        let timer () =
          let tcp, drops, outs = Tcp.Tcptimer.timer t.tcp (now ()) in
          t.tcp <- tcp;
          List.iter (fun id ->
              match Tcp.State.CM.find_opt id t.waiting with
              | None -> Log.warn (fun m -> m "%a not found in waiting"
                                     Tcp.State.Connection.pp id)
              | Some c ->
                Lwt_condition.broadcast c (Error (`Msg "timer timed out")))
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

  let disconnect _t =
    Lwt.return_unit
end
