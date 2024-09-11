
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

let print_out (src, dst, seg) =
  Logs.info (fun m -> m "sending %a -> %a: %a" Ipaddr.pp src Ipaddr.pp dst
                Utcp.Segment.pp seg)

let initial_packet state ip rng_data now ~src ~dst tcp =
  let src_port = Cstruct.BE.get_uint16 tcp 0
  and dst_port = Cstruct.BE.get_uint16 tcp 2
  in
  if Ipaddr.compare ip src = 0 then begin
    (* we're the client - set ISS to match *)
    Bytes.set_int32_le rng_data 0 (Cstruct.BE.get_uint32 tcp 4);
    let state, flow, _cond, out =
      Utcp.connect ~src ~src_port ~dst ~dst_port state now
    in
    print_out out;
    state, None, Some flow
  end else begin
    (* we're the server - don't know our ISS yet *)
    let state = Utcp.start_listen state dst_port in
    let f state ~src:src' ~dst:dst' tcp' =
      (* ensure we got the other direction *)
      assert (Ipaddr.compare dst src' = 0);
      assert (Ipaddr.compare ip src' = 0);
      assert (Ipaddr.compare src dst' = 0);
      assert (dst_port = Cstruct.BE.get_uint16 tcp' 0);
      assert (src_port = Cstruct.BE.get_uint16 tcp' 2);
      (* set ISS to the seq in the next segment ;) *)
      Bytes.set_int32_le rng_data 0 (Cstruct.BE.get_uint32 tcp' 4);
      (* replay the 0 packet *)
      Logs.info (fun m -> m "replay packet 0");
      let state, _stuff, out = Utcp.handle_buf state now ~src ~dst tcp in
      List.iter print_out out;
      state
    in
    state, Some f, None
  end

let jump () filename ip =
  if ip = None then
    Logs.warn (fun m -> m "only decoding and printing pcap, no replaying done (specify --ip=<IP> to take an endpoint)");
  let fold = pcap_reader filename in
  let rng_data = Bytes.make 4 '\000' in
  let rng i = assert (i = 4) ; Bytes.unsafe_to_string rng_data in
  let state = Utcp.empty Fun.id "pcap-replay" rng in
  let flow = ref None in
  fold (fun (state, act) idx ts ~src ~dst tcp ->
      let state =
        match act with
        | None -> state
        | Some f -> f state ~src ~dst tcp
      in
      Logs.info (fun m -> m "frame %u ts %a src %a dst %a tcp %u bytes"
                    idx Mtime.Span.pp ts Ipaddr.pp src Ipaddr.pp dst (Cstruct.length tcp));
      let mt = Mtime.of_uint64_ns (Mtime.Span.to_uint64_ns ts) in
      match ip with
      | Some x ->
        if idx = 0 then
          let state, f, fl = initial_packet state x rng_data mt ~src ~dst tcp in
          flow := fl;
          state, f
        else if Ipaddr.compare x dst = 0 then begin
          Logs.info (fun m -> m "replay packet %u" idx);
          let state, stuff, out = Utcp.handle_buf state mt ~src ~dst tcp in
          (match stuff with Some (`Established (fl, _)) ->
             Logs.info (fun m -> m "flow established! (previously %s)"
                           (match !flow with Some _ -> "SOME" | None -> "none"));
             flow := Some fl | _ -> ());
          List.iter print_out out;
          state, None
        end else if Ipaddr.compare x src = 0 then begin
          (* NB: we need to inject the read/write/close calls somehow *)
          (* TODO: check that the respective state matches the sequence numbers in the pcap *)
          let payload =
            let off = (Cstruct.get_uint8 tcp 12 lsr 4) * 4 in
            Cstruct.shift tcp off
          in
          let syn, fin =
            let flags = Cstruct.get_uint8 tcp 13 in
            let f = (1 lsl 0) land flags > 0
            and s = (1 lsl 1) land flags > 0
            in
            s, f
          in
          let state =
            if syn then begin
              Logs.warn (fun m -> m "weird, packet %u carries a SYN, ignoring" idx);
              state
            end else if fin || Cstruct.length payload > 0 then
              let flow = Option.get !flow in
              let state =
                if Cstruct.length payload > 0 then (
                  Logs.info (fun m -> m "sending with %u bytes" (Cstruct.length payload));
                  match Utcp.send state mt flow payload with
                  | Error `Msg msg ->
                    Logs.err (fun m -> m "failure during send: %s" msg);
                    assert false
                  | Ok (state, bytes_sent, _cond, out) ->
                    if bytes_sent <> Cstruct.length payload then begin
                      Logs.err (fun m -> m "partial send: %u of %u bytes"
                                   bytes_sent (Cstruct.length payload));
                      assert false
                    end;
                    List.iter print_out out;
                    state
                ) else
                  state
              in
              if fin then
                (Logs.info (fun m -> m "close");
                 match Utcp.close state mt flow with
                | Error `Msg msg ->
                  Logs.err (fun m -> m "failure during close: %s" msg);
                  assert false
                | Ok (state, out) ->
                  List.iter print_out out;
                  state)
              else
                state
            else state
          in
          state, None
        end else begin
          Logs.info (fun m -> m "ignoring packet (neither src %a nor dst %a are us %a) %u"
                        Ipaddr.pp src Ipaddr.pp dst Ipaddr.pp x idx);
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
