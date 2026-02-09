open Lwt.Infix

module Ethernet = Ethernet.Make(Netif)
module ARP = Arp.Make(Ethernet)
module IPv4 = Static_ipv4.Make(Ethernet)(ARP)

let log_err ~pp_error = function
  | Ok _ -> ()
  | Error e -> Logs.err (fun m -> m "error %a" pp_error e)

let handle_data ip =
  Lwt_list.iter_s (fun (src, dst, seg) ->
      let data = Utcp.Segment.encode_and_checksum (Mtime_clock.now ()) ~src ~dst seg in
      match src, dst with
      | Ipaddr.V4 _, Ipaddr.V4 dst ->
        IPv4.write ip dst `TCP (fun _ -> 0) [ data ] >|=
        log_err ~pp_error:IPv4.pp_error
      | Ipaddr.V6 _, Ipaddr.V6 _ ->
        Lwt.return (Logs.err (fun m -> m "IPv6 not supported at the moment"))
      | _ -> invalid_arg "bad sportsmanship")

let cb ~proto ~src ~dst payload =
  Logs.app (fun m -> m "received proto %X frame %a -> %a (%d bytes)" proto
               Ipaddr.V4.pp src Ipaddr.V4.pp dst (Cstruct.length payload));
  Lwt.return_unit

let jump () =
  Printexc.record_backtrace true;
  Lwt_main.run (
    Mirage_crypto_rng_unix.use_default ();
    Netif.connect "tap2" >>= fun tap ->
    Ethernet.connect tap >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    let cidr = Ipaddr.V4.Prefix.of_string_exn "10.0.42.2/24" in
    IPv4.connect ~cidr eth arp >>= fun ip ->
    let tcp (*, clo, out *) =
      (* let dst = Ipaddr.(V4 (V4.of_string_exn "10.0.42.1")) in *)
      let init (*, conn, out *) =
        let s = Utcp.empty Fun.id "" in
        let s' = Utcp.start_listen s 23 in
        (* Tcp.connect ~src:Ipaddr.(V4 (V4.Prefix.address cidr)) ~dst ~dst_port:1234 s' (Mtime_clock.now ()) *)
        s'
      in
      let s = ref init in
      let _ = Lwt_engine.on_timer 0.1 true (fun _ ->
          let s', _drops, outs = Utcp.timer !s (Mtime_clock.now ()) in
          s := s' ;
          Lwt.async (fun () -> handle_data ip outs))
      in
      (fun ~src ~dst payload ->
         let src = Ipaddr.V4 src and dst = Ipaddr.V4 dst in
         let s', ev, data =
           Utcp.handle_buf !s (Mtime_clock.now ()) ~src ~dst payload
         in
         (match ev with
          | Some `Established _id -> Logs.app (fun m -> m "connection established")
          | Some `Drop _id -> Logs.app (fun m -> m "connection drop")
          | Some `Signal (_id, _) -> Logs.app (fun m -> m "buffer")
          | None -> ());
         s := s' ;
         handle_data ip data) (*,
      (fun () ->
         match Tcp.close !s conn with
         | Error (`Msg msg) -> Logs.err (fun m -> m "close failed %s" msg)
         | Ok s' -> s := s'b),
         (dst, out) *)
    in
    let eth_input =
      Ethernet.input eth
        ~arpv4:(ARP.input arp)
        ~ipv4:(IPv4.input ip ~tcp ~udp:(cb ~proto:17) ~default:cb)
        ~ipv6:(fun _ -> Lwt.return_unit)
    in
    (* delay client a bit to have arp up and running *)
(*    Lwt.async (fun () ->
        Lwt_unix.sleep 2. >>= fun () ->
        handle_events ip [ `Data out ] >>= fun () ->
        Lwt_unix.sleep 1. >|= fun () ->
        Logs.info (fun m -> m "closing!!");
        clo ()); *)
    Netif.listen tap ~header_size:14 eth_input >|=
    Result.map_error (fun e -> `Msg (Fmt.to_to_string Netif.pp_error e))
  )

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let cmd =
  let term = Term.(term_result (const jump $ setup_log))
  and info = Cmd.info "server" ~version:"%%VERSION_NUM%%"
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
