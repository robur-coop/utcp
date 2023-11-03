
let pcap_reader filename =
  let data =
    try
      let fh = open_in filename in
      let content = really_input_string fh (in_channel_length fh) in
      close_in_noerr fh;
      content
    with _ -> invalid_arg ("Error reading file " ^ filename)
  in
  let data = Cstruct.of_string data in
  let header, body = Cstruct.split data Pcap.sizeof_pcap_header in
  let hdr = Option.get (Pcap.detect header) in
  let module H = (val hdr : Pcap.HDR) in
  let iter = Pcap.packets hdr body in
  let rec go_along idx ts f acc =
    match iter () with
    | None -> ()
    | Some (hdr, body) ->
      let ts, start =
        let sec = H.get_pcap_packet_ts_sec hdr
        and usec = H.get_pcap_packet_ts_usec hdr
        in
        let ns = Int64.(mul (add (mul (of_int32 sec) 1_000_000L) (of_int32 usec)) 1_000L) in
        let ts' = Mtime.of_uint64_ns ns in
        if Mtime.equal (Mtime.of_uint64_ns 0L) ts then
          Mtime.Span.zero, ts'
        else
          Mtime.span ts ts', ts
      in
      let ip = Cstruct.shift body 14 in
      let src_dst_tcp =
        let ethertype = Cstruct.BE.get_uint16 body 12 in
        if ethertype = 0x0800 && Cstruct.length ip >= 20 then (* ipv4 *)
          if Cstruct.get_uint8 ip 9 = 6 then (* TCP *)
            let hlen = (Cstruct.get_uint8 ip 0 land 0x0f) * 4 in
            Some (Ipaddr.V4 (Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 ip 12)),
                  Ipaddr.V4 (Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 ip 16)),
                  Cstruct.shift ip hlen)
          else begin
            Logs.warn (fun m -> m "ignoring IPv4 packet with protocol %2X" (Cstruct.get_uint8 ip 9));
            None
          end
        else if ethertype = 0x86dd && Cstruct.length ip >= 40 then (* ipv6 *)
          let rec find_protocol hdr buf =
            if Cstruct.length buf < 2 then
              `Exceeded
            else if hdr = 6 then (* TCP *)
              `Tcp buf
            else
              let hdr = Cstruct.get_uint8 buf 0
              and buf = Cstruct.shift buf (Cstruct.get_uint8 buf 1)
              in
              find_protocol hdr buf
          in
          match find_protocol (Cstruct.get_uint8 ip 6) (Cstruct.shift ip 40) with
          | `Tcp buf ->
            Some (Ipaddr.V6 (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift ip 8)),
                  Ipaddr.V6 (Ipaddr_cstruct.V6.of_cstruct_exn (Cstruct.shift ip 24)),
                  buf)
          | _ ->
            Logs.warn (fun m -> m "ignoring IPv6 packet without TCP payload");
            None
        else begin
          Logs.warn (fun m -> m "ignoring unknown ethertype %4X" ethertype);
          None
        end
      in
      let acc' =
        match src_dst_tcp with
        | Some (src, dst, tcp) -> f acc idx ts ~src ~dst tcp
        | None -> acc
      in
      go_along (succ idx) start f acc'
  in
  go_along 0 (Mtime.of_uint64_ns 0L)

let jump () filename ip =
  if ip = None then
    Logs.warn (fun m -> m "only decoding and printing pcap, no replaying done (specify --ip=<IP> to take an endpoint)");
  let fold = pcap_reader filename in
  let rng_data = Cstruct.create 4 in
  let rng i = assert (i = 4) ; rng_data in
  let state = Utcp.empty "pcap-replay" rng in
  fold (fun (state, act) idx ts ~src ~dst tcp ->
      let state =
        match act with
        | None -> state
        | Some f -> f state ~src ~dst tcp
      in
      Logs.info (fun m -> m "frame %u ts %a src %a dst %a tcp %u bytes"
                    idx Mtime.Span.pp ts Ipaddr.pp src Ipaddr.pp dst (Cstruct.length tcp));
      let src_port = Cstruct.BE.get_uint16 tcp 0
      and dst_port = Cstruct.BE.get_uint16 tcp 2
      in
      let mt = Mtime.of_uint64_ns (Mtime.Span.to_uint64_ns ts) in
      match ip with
      | Some x ->
        if idx = 0 then
          if Ipaddr.compare x src = 0 then begin
            (* we're the client - set ISS to match *)
            Cstruct.LE.set_uint32 rng_data 0 (Cstruct.BE.get_uint32 tcp 4);
            let state, _flow, _out =
              Utcp.connect ~src ~src_port ~dst ~dst_port state mt
            in
            state, None
          end else begin
            (* we're the server - don't know our ISS yet *)
            let state = Utcp.start_listen state dst_port in
            let f state ~src:src' ~dst:dst' tcp' =
              (* ensure we got the other direction *)
              assert (Ipaddr.compare dst src' = 0);
              assert (Ipaddr.compare x src' = 0);
              assert (Ipaddr.compare src dst' = 0);
              assert (dst_port = Cstruct.BE.get_uint16 tcp' 0);
              assert (src_port = Cstruct.BE.get_uint16 tcp' 2);
              (* set ISS to the seq in the next segment ;) *)
              Cstruct.LE.set_uint32 rng_data 0 (Cstruct.BE.get_uint32 tcp' 4);
              (* replay the 0 packet *)
              Logs.info (fun m -> m "replay packet 0");
              let state, _stuff, _out = Utcp.handle_buf state mt ~src ~dst tcp in
              state
            in
            state, Some f
          end
        else if Ipaddr.compare x dst = 0 then begin
          Logs.info (fun m -> m "replay packet %u" idx);
          let state, _stuff, _out = Utcp.handle_buf state mt ~src ~dst tcp in
          state, None
        end else begin
          (* NB: we need to inject the read/write/close calls somehow *)
          (* also could check that the respective state matches the sequence numbers in the pcap *)
          (* or actually implement a reasonable tracing approach in utcp for replaying *)
          Logs.info (fun m -> m "ignoring packet %u" idx);
          state, None
        end
      | _ -> state, None
      )
      (state, None);
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let pcap_file =
  let doc = "File name of PCAP file." in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"PCAP")

let ip =
  let doc = "IP address we want to mimic." in
  Arg.(value & opt (some (Arg.conv (Ipaddr.of_string, Ipaddr.pp))) None &
       info [ "ip" ] ~doc ~docv:"IP")

let cmd =
  let term = Term.(term_result (const jump $ setup_log $ pcap_file $ ip))
  and info = Cmd.info "pcap_replay" ~version:"%%VERSION_NUM%%"
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
