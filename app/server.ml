open Lwt.Infix

module Ethernet = Ethernet.Make(Netif)
module ARP = Arp.Make(Ethernet)(OS.Time)
module IPv4 = Static_ipv4.Make(Mirage_random_test)(Mclock)(Ethernet)(ARP)

let log_err ~pp_error = function
  | Ok _ -> ()
  | Error e -> Logs.err (fun m -> m "error %a" pp_error e)

let handle_events ip =
  Lwt_list.iter_s (function
      | `Data (dst, out) ->
        IPv4.write ip dst `TCP (fun _ -> 0) [ out ] >|=
        log_err ~pp_error:IPv4.pp_error)

let cb ~proto ~src ~dst payload =
  Logs.app (fun m -> m "received proto %X frame %a -> %a (%d bytes)" proto
               Ipaddr.V4.pp src Ipaddr.V4.pp dst (Cstruct.len payload));
  Lwt.return_unit

let jump () =
  Printexc.record_backtrace true;
  Mirage_random_test.initialize ();
  Lwt_main.run (
    Netif.connect "tap1" >>= fun tap ->
    Ethernet.connect tap >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    let ip_addr = Ipaddr.V4.of_string_exn "10.0.42.2"
    and network = Ipaddr.V4.Prefix.of_string_exn "10.0.42.2/24"
    in
    IPv4.connect ~ip:ip_addr ~network () eth arp >>= fun ip ->
    let tcp (*, clo, out *) =
      (* let dst = Ipaddr.V4.of_string_exn "10.0.42.1" in *)
      let init (*, conn, out *) =
        let s = Tcp.State.empty Mirage_random_test.generate ip_addr in
        let s' = Tcp.State.start_listen s 23 in
        (* Tcp.User.connect s' (Mtime_clock.now ()) dst 1234 *)
        s'
      in
      let s = ref init in
      (fun ~src ~dst payload ->
         let s', events = Tcp.Input.handle !s (Mtime_clock.now ()) ~src ~dst payload in
         s := s' ;
         handle_events ip events) (*,
      (fun () ->
         match Tcp.User.close !s conn with
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
    Rresult.R.error_to_msg ~pp_error:Netif.pp_error
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
  Term.(term_result (const jump $ setup_log)),
  Term.info "server" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
