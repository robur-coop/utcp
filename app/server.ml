open Lwt.Infix

module Ethernet = Ethernet.Make(Netif)
module ARP = Arp.Make(Ethernet)(OS.Time)
module IPv4 = Static_ipv4.Make(Mirage_random_test)(Mclock)(Ethernet)(ARP)

let tcp_state = ref Tcp.State.empty

let tcp ~src ~dst payload =
  Logs.app (fun m -> m "received TCP frame %a -> %a (%d bytes)"
               Ipaddr.V4.pp src Ipaddr.V4.pp dst (Cstruct.len payload));
  (match Tcp.Input.handle !tcp_state ~src ~dst payload with
   | Ok (tcp_state', _) -> tcp_state := tcp_state'
   | Error (`Msg msg) -> Logs.err (fun m -> m "invalid frame %s" msg));
  Lwt.return_unit

let cb ~proto ~src ~dst payload =
  Logs.app (fun m -> m "received proto %X frame %a -> %a (%d bytes)" proto
               Ipaddr.V4.pp src Ipaddr.V4.pp dst (Cstruct.len payload));
  Lwt.return_unit

let jump () =
  Lwt_main.run (
    Netif.connect "tap1" >>= fun tap ->
    Ethernet.connect tap >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    IPv4.connect
      ~ip:(Ipaddr.V4.of_string_exn "10.0.42.2")
      ~network:(Ipaddr.V4.Prefix.of_string_exn "10.0.42.2/24")
      () eth arp >>= fun ip ->
    let eth_input =
      Ethernet.input eth
        ~arpv4:(ARP.input arp)
        ~ipv4:(IPv4.input ip ~tcp ~udp:(cb ~proto:17) ~default:cb)
        ~ipv6:(fun _ -> Lwt.return_unit)
    in
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
