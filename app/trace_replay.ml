
let ( let* ) = Result.bind

type func = [
  | `Handle_buf
  | `Out
  | `Receive
  | `Send
  | `Close
  | `Connect
  | `Shutdown of [ `read | `write | `read_write ]
]

let has_data = function
  | `Handle_buf -> true
  | `Out -> true
  | `Receive -> false
  | `Send -> true
  | `Close -> false
  | `Connect -> false
  | `Shutdown _ -> false

let func_of_string = function
  | "handle_buf" -> Ok `Handle_buf
  | "out" -> Ok `Out
  | "receive" -> Ok `Receive
  | "send" -> Ok `Send
  | "close" -> Ok `Close
  | "connect" -> Ok `Connect
  | "shutdown_read" -> Ok (`Shutdown `read)
  | "shutdown_write" -> Ok (`Shutdown `write)
  | "shutdown_readwrite" -> Ok (`Shutdown `read_write)
  | f -> Error (`Msg ("unknown function " ^ f))

let func_to_string = function
  | `Handle_buf -> "handle_buf"
  | `Out -> "out"
  | `Receive -> "receive"
  | `Send -> "send"
  | `Close -> "close"
  | `Connect -> "connect"
  | `Shutdown _ -> "shutdown"

type event = {
  flow : (Ipaddr.t * int) * (Ipaddr.t * int) ;
  ts : Mtime.Span.t ;
  func : func ;
  data : int * string option ;
}

let pp_event start_ts ppf { ts ; func ; data ; _ } =
  Fmt.pf ppf "%a %s%s" Mtime.Span.pp (Mtime.Span.abs_diff ts start_ts)
    (func_to_string func)
    (if fst data = 0 then "" else ", " ^ string_of_int (fst data) ^ " bytes payload")

let trace_reader filename =
  let data =
    try
      let fh = open_in filename in
      let content = really_input_string fh (in_channel_length fh) in
      close_in_noerr fh;
      content
    with _ -> invalid_arg ("Error reading file " ^ filename)
  in
  let lines = String.split_on_char '\n' data in
  let msgs =
    let decode_flow src dst =
      let decode_ip_port a =
        match List.rev (String.split_on_char ':' a) with
        | port :: rest ->
          let* ip = Ipaddr.of_string (String.concat ":" (List.rev rest)) in
          let* port =
            try Ok (int_of_string port) with
              Failure _ -> Error (`Msg ("not a port number " ^ port))
          in
          if port >= 0 && port <= 65535 then
            Ok (ip, port)
          else
            Error (`Msg ("port not in range " ^ string_of_int port))
        | [] -> Error (`Msg ("bad ip:port " ^ a))
      in
      let* src = decode_ip_port src in
      let* dst = decode_ip_port dst in
      Ok (src, dst)
    in
    let decode_ts data =
      if String.length data > 4 &&
         (String.get data 0 = '[') &&
         (String.get data (String.length data - 1) = ']') &&
         (String.sub data (String.length data - 3) 2 = "ns")
      then
        let* ns =
          let s = String.sub data 1 (String.length data - 4) in
          try Ok (Int64.of_string s)
          with Failure _ -> Error (`Msg ("couldn't decode timestamp " ^ s))
        in
        Ok (Mtime.Span.of_uint64_ns ns)
      else
        Error (`Msg ("failed timestamp format, needs [<numbers>ns], got " ^ data))
    in
    let decode_msg idx = function
      | src::"->"::dst::ts::func::data ->
        let* flow = decode_flow src dst in
        let* ts = decode_ts ts in
        let* func = func_of_string func in
        let* data =
          if has_data func then
            match data with
            | [rest] ->
              begin match Base64.decode rest with
                | Ok data ->
                  let len = String.length data in
                  let len = if func = `Out then len - 20 (* tcp header *) else len in
                  Ok (len, Some data)
                | Error _ -> (* guess or consult the pcap?, truncated *)
                  if String.ends_with ~suffix:"fw2-1.log" filename then
                    match idx with
                    | 11 -> Ok (4119, None)
                    | 30 -> Ok (5507, None)
                    | _ -> Error (`Msg "not length-prefixed trace, and decode error")
                  else
                    Error (`Msg "not length-prefixed trace, and decode error")
              end
            | [len ; rest] ->
              let* len =
                try Ok (int_of_string len) with
                  Failure _ -> Error (`Msg "couldn't decode data length")
              in
              begin match Base64.decode rest with
                | Ok data -> Ok (len, Some data)
                | Error _ -> Ok (len, None) (* truncation *)
              end
            | _ -> Error (`Msg ("awaited one or two elements for data, got " ^ string_of_int (List.length data)))
          else
            Ok (0, None)
        in
        Ok { flow ; ts ; func ; data }
      | _ -> Error (`Msg "bad format")
    in
    List.fold_left (fun (acc, idx) line ->
        match
          match String.split_on_char ' ' line |> List.filter (function "" -> false | _ -> true) with
          | _m::_d::_ts::_fac::_ip::"tcp.tracing"::rest -> decode_msg idx rest
          | _m::_d::_ts::_fac::_ip::src::_ -> Error (`Msg2 ("log source is " ^ src))
          | _ -> Error (`Msg2 "unexpected line")
        with
        | Ok msg -> msg :: acc, succ idx
        | Error `Msg thing ->
          Logs.warn (fun m -> m "dismissing %s %u (%S)" thing idx line);
          acc, succ idx
        | Error `Msg2 _ -> acc, succ idx)
      ([], 0) lines |> fst |> List.rev
  in
  Logs.info (fun m -> m "decoded %u lines" (List.length msgs));
  msgs

let print_out (src, dst, seg) =
  Logs.info (fun m -> m "sending %a -> %a: %a" Ipaddr.pp src Ipaddr.pp dst
                Utcp.Segment.pp seg)

let send_out_good msgs =
  match
    List.fold_left (fun (act, idx) { func ; data ; _ } ->
        match act with
        | Error _ as e -> e, succ idx
        | Ok act ->
        let r =
          match act with
          | None ->
            (match func with
             | `Send -> Ok (Some (fst data))
             | _ -> Ok None)
          | Some req ->
            (match func with
             | `Out ->
               let left = req - fst data in
               if left < 0 then
                 Error (`Msg ("too much out in " ^ string_of_int idx ^ ": " ^ string_of_int left))
               else if left = 0 then
                 Ok None
               else
                 Ok (Some left)
             | _ -> Error (`Msg ("expected out in " ^ string_of_int idx ^ ": " ^ string_of_int req)))
        in
        r, succ idx)
      (Ok None, 1) msgs |> fst
  with
  | Ok None -> Ok ()
  | Ok Some _ -> Error (`Msg "leftover action")
  | Error _ as e -> e

let print start_ts idx event =
  Logs.app (fun m -> m "#%u at %a" idx (pp_event start_ts) event)

let jump () filename ip =
  if ip = None then
    Logs.warn (fun m -> m "only decoding and printing trace, no replaying done (specify --ip=<IP> to take an endpoint)");
  let msgs = trace_reader filename in
  let rng_data = Cstruct.create 4 in
  let rng i = assert (i = 4) ; rng_data in
  let state =
    let s = Utcp.empty "trace-replay" rng in
    Utcp.start_listen s 443
  in
  let flow = ref None in
  let* () = send_out_good msgs in
  let start_ts = (List.hd msgs).ts in
  let () =
    let first_out =
      List.filter (fun { func ; _ } -> if func = `Out then true else false) msgs
      |> List.hd
    in
    let data = Option.get (snd first_out.data) in
    Cstruct.LE.set_uint32 rng_data 0
      (Cstruct.BE.get_uint32 (Cstruct.of_string data) 4)
  in
  let _, _ , _ =
    List.fold_left (fun (state, out_after_send, idx) msg ->
        print start_ts idx msg;
        let src, dst = fst (fst msg.flow), fst (snd msg.flow) in
        let tm = Mtime.of_uint64_ns Mtime.Span.(to_uint64_ns msg.ts) in
        match msg.func with
        | `Handle_buf ->
          let data = Cstruct.of_string (Option.get (snd msg.data)) in
          let state, stuff, out = Utcp.handle_buf state tm ~src ~dst data in
          (match stuff with Some `Established fl ->
             Logs.info (fun m -> m "flow established! (previously %s)"
                           (match !flow with Some _ -> "SOME" | None -> "none"));
             flow := Some fl | _ -> ());
          List.iter print_out out;
          state, false, succ idx
        | `Out when out_after_send ->
          Logs.app (fun m -> m "ignoring out after send");
          state, out_after_send, succ idx
        | `Out ->
          Logs.app (fun m -> m "ignoring out");
          state, out_after_send, succ idx
        | `Receive ->
          let flow = Option.get !flow in
          (match Utcp.recv state tm flow with
           | Ok (state, _data, out) ->
             List.iter print_out out;
             state, false, succ idx
           | Error `Msg s ->
             Logs.err (fun m -> m "recv error %s" s);
             state, false, succ idx
           | Error `Eof ->
             Logs.err (fun m -> m "recv eof");
             state, false, succ idx)
        | `Send ->
          let flow = Option.get !flow in
          (match Utcp.send state tm flow (Cstruct.create (fst msg.data)) with
           | Ok (state, out) ->
             List.iter print_out out;
             state, true, succ idx
           | Error `Msg s ->
             Logs.err (fun m -> m "send error %s" s);
             state, true, succ idx)
        | `Close ->
          let flow = Option.get !flow in
          (match Utcp.close state tm flow with
           | Ok (state, out) ->
             List.iter print_out out;
             state, true, succ idx
           | Error `Msg s ->
             Logs.err (fun m -> m "close error %s" s);
             state, true, succ idx)
        | _ -> assert false)
      (state, false, 0) msgs
  in
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

let trace_file =
  let doc = "File name of TRACE file." in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv:"TRACE")

let ip =
  let doc = "IP address we want to mimic." in
  Arg.(value & opt (some (Arg.conv (Ipaddr.of_string, Ipaddr.pp))) None &
       info [ "ip" ] ~doc ~docv:"IP")

let cmd =
  let term = Term.(term_result (const jump $ setup_log $ trace_file $ ip))
  and info = Cmd.info "trace_replay" ~version:"%%VERSION_NUM%%"
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
