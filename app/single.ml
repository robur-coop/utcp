open Lwt.Infix

module Ethernet = Ethernet.Make(Netif)
module ARP = Arp.Make(Ethernet)(Unix_os.Time)
module R = Mirage_crypto_rng_mirage.Make(Unix_os.Time)(Mclock)
module IPv4 = Static_ipv4.Make(R)(Mclock)(Ethernet)(ARP)

let log_err ~pp_error = function
  | Ok _ -> ()
  | Error e -> Logs.err (fun m -> m "error %a" pp_error e)

let send_tcp ip dst out =
  match dst with
  | Ipaddr.V4 dst ->
    IPv4.write ip dst `TCP (fun _ -> 0) [ out ] >|=
    log_err ~pp_error:IPv4.pp_error
  | Ipaddr.V6 _ ->
    Lwt.return (Logs.err (fun m -> m "IPv6 not supported at the moment"))

let cb ~proto ~src ~dst payload =
  Logs.app (fun m -> m "received proto %X frame %a -> %a (%d bytes)" proto
               Ipaddr.V4.pp src Ipaddr.V4.pp dst (Cstruct.length payload));
  Lwt.return_unit

let tcp_cb ~src ~dst payload =
  let src = Ipaddr.V4 src and dst = Ipaddr.V4 dst in
  (match Utcp.Segment.decode_and_validate ~src ~dst payload with
   | Error (`Msg msg) -> Logs.app (fun m -> m "TCP received, error %s" msg)
   | Ok (s, id) -> Logs.app (fun m -> m "%a TCP %a" Utcp.pp_flow id Utcp.Segment.pp s)) ;
  Lwt.return_unit

let jump _ src src_port dst dst_port syn fin rst push ack seq window data =
  Printexc.record_backtrace true;
  Lwt_main.run (
    R.initialize (module Mirage_crypto_rng.Fortuna) >>= fun () ->
    let cidr = Ipaddr.V4.Prefix.of_string_exn src
    and dst = Ipaddr.(V4 (V4.of_string_exn dst))
    in
    let seg =
      let open Utcp in
      let open Segment in
      let flag =
        match syn, fin, rst with
        | true, false, false -> Some `Syn
        | false, true, false -> Some `Fin
        | false, false, true -> Some `Rst
        | false, false, false -> None
        | _ -> invalid_arg "invalid flag combination"
      and ack = match ack with None -> None | Some x -> Some (Sequence.of_int32 (Int32.of_int x))
      and payload = match data with None -> Cstruct.empty | Some x -> Cstruct.of_string x
      in
      let s = {
        src_port ; dst_port ;
        seq = Sequence.of_int32 (Int32.of_int seq) ;
        ack ; flag ; push ; window ; options = [] ; payload
      } in
      encode_and_checksum (Mtime_clock.now ()) ~src:Ipaddr.(V4 (V4.Prefix.address cidr)) ~dst s
    in
    Netif.connect "tap3" >>= fun tap ->
    Ethernet.connect tap >>= fun eth ->
    ARP.connect eth >>= fun arp ->
    IPv4.connect ~cidr eth arp >>= fun ip ->
    let eth_input =
      Ethernet.input eth
        ~arpv4:(ARP.input arp)
        ~ipv4:(IPv4.input ip ~tcp:tcp_cb ~udp:(cb ~proto:17) ~default:cb)
        ~ipv6:(fun _ -> Lwt.return_unit)
    in
    (* delay client a bit to have arp up and running *)
    Lwt.async (fun () ->
        Lwt_unix.sleep 2. >>= fun () ->
        send_tcp ip dst seg);
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

let syn = Arg.(value & flag & info [ "syn" ] ~doc:"Set syn flag")
let fin = Arg.(value & flag & info [ "fin" ] ~doc:"Set fin flag")
let rst = Arg.(value & flag & info [ "rst" ] ~doc:"Set rst flag")
let psh = Arg.(value & flag & info [ "psh" ] ~doc:"Set psh flag")

let ack =
  Arg.(value & opt (some int) None & info [ "ack" ] ~doc:"Set acknowledgement")

let seq =
  Arg.(value & opt int 42 & info [ "seq" ] ~doc:"Sequence number")

let window =
  Arg.(value & opt int 65535 & info [ "window" ] ~doc:"Window size")

let src_port =
  Arg.(value & opt int 12345 & info [ "src-port" ] ~doc:"Source port")

let dst_port =
  Arg.(value & opt int 80 & info [ "dst-port" ] ~doc:"Destination port")

let src =
  Arg.(value & opt string "10.0.42.2/24" & info [ "src" ] ~doc:"Source IP and netmask")

let dst =
  Arg.(value & opt string "10.0.42.1" & info [ "dst" ] ~doc:"Destination IP")

let data =
  Arg.(value & opt (some string) None & info [ "data" ] ~doc:"Data to transmit")

let cmd =
  let term =
    Term.(term_result (const jump $ setup_log $ src $ src_port $ dst $ dst_port $ syn $ fin $ rst $ psh $ ack $ seq $ window $ data))
  and info = Cmd.info "single" ~version:"%%VERSION_NUM%%"
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
