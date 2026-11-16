(* Two utcp stacks talking to each other: every segment one of them outputs is
   delivered to the other. Delivery happens in rounds (all segments in flight
   in both directions are delivered "at the same time"), so segments can cross
   on the wire, as in a simultaneous close. *)

open Utcp

let ip_a = Ipaddr.of_string_exn "10.0.0.1"
let ip_b = Ipaddr.of_string_exn "10.0.0.2"
let port_b = 443

let now = ref (Mtime.of_uint64_ns 1_000_000_000L)
let tick ms = now := Mtime.of_uint64_ns (Int64.add (Mtime.to_uint64_ns !now) (Int64.mul 1_000_000L ms))

type side = { mutable st : unit state ; ip : Ipaddr.t ; mutable flow : flow option }

let a () = { st = empty (fun () -> ()) "a" ; ip = ip_a ; flow = None }
let b () = { st = start_listen (empty (fun () -> ()) "b") port_b ; ip = ip_b ; flow = None }

let segments = ref 0

(* in flight, per destination *)
let to_a = ref [] and to_b = ref []

let queue outs =
  List.iter (fun (src, dst, seg) ->
      let buf = Segment.encode_and_checksum !now ~src ~dst seg in
      if Ipaddr.compare dst ip_a = 0 then to_a := !to_a @ [ (src, dst, buf) ]
      else to_b := !to_b @ [ (src, dst, buf) ])
    outs

let deliver side bufs =
  List.iter (fun (src, dst, buf) ->
      incr segments;
      let st, evs, outs = handle_buf side.st !now ~src ~dst buf in
      side.st <- st;
      List.iter (function
          | `Established (flow, _) -> side.flow <- Some flow
          | _ -> ()) evs;
      queue outs)
    bufs

(* one round: everything in flight, both ways, crosses at once *)
let round sa sb =
  let for_a = !to_a and for_b = !to_b in
  to_a := [] ; to_b := [];
  deliver sa for_a ; deliver sb for_b

let run ?(limit = 1000) sa sb =
  let start = !segments in
  while (!to_a <> [] || !to_b <> []) && !segments - start < limit do
    round sa sb
  done;
  !segments - start

let establish () =
  segments := 0; to_a := []; to_b := [];
  let sa = a () and sb = b () in
  (match connect ~src:ip_a ~dst:ip_b ~dst_port:port_b sa.st !now with
   | Ok (st, flow, (), out) -> sa.st <- st ; sa.flow <- Some flow ; queue [ out ]
   | Error (`Msg m) -> Alcotest.fail m);
  ignore (run sa sb);
  Alcotest.(check bool) "established" true (sa.flow <> None && sb.flow <> None);
  sa, sb

let flow s = Option.get s.flow

let send s data =
  match send s.st !now (flow s) data with
  | Ok (st, _, (), outs) -> s.st <- st ; queue outs
  | Error e -> Alcotest.failf "send: %a" pp_error e

let recv s =
  match recv s.st !now (flow s) with
  | Ok (st, _, (), outs) -> s.st <- st ; queue outs
  | Error `Eof -> ()
  | Error (#error as e) -> Alcotest.failf "recv: %a" pp_error e

let close s =
  match close s.st !now (flow s) with
  | Ok (st, _, outs) -> s.st <- st ; queue outs
  | Error e -> Alcotest.failf "close: %a" pp_error e

(* a closing handshake needs a handful of segments, not thousands *)
let check_quiet name n =
  if n >= 50 then Alcotest.failf "%s: %d segments exchanged, still going" name n

(* fire the timers for a while (retransmissions, 2MSL) and check that the
   pair stays quiet and eventually forgets the connection *)
let drain sa sb =
  for _ = 1 to 300 do
    tick 1000L;
    let st, _, outs = timer sa.st !now in sa.st <- st ; queue outs;
    let st, _, outs = timer sb.st !now in sb.st <- st ; queue outs;
    check_quiet "after a timer" (run sa sb)
  done;
  Alcotest.(check int) "a: no connections left" 0 (num_connections sa.st);
  Alcotest.(check int) "b: no connections left" 0 (num_connections sb.st)

let simultaneous_close_no_data () =
  let sa, sb = establish () in
  close sa ; close sb;
  check_quiet "simultaneous close" (run sa sb);
  drain sa sb

let simultaneous_close_after_exchange () =
  let sa, sb = establish () in
  send sa (String.make 517 'q'); ignore (run sa sb); recv sb;
  send sb (String.make 4000 'r'); ignore (run sa sb); recv sa;
  ignore (run sa sb);
  close sa ; close sb;
  check_quiet "simultaneous close" (run sa sb);
  drain sa sb

(* rainbow: data (TLS close_notify) and FIN in one segment, while the client
   (status) closes at the same time *)
let data_fin_crossing_fin () =
  let sa, sb = establish () in
  send sa (String.make 300 'q'); ignore (run sa sb); recv sb;
  ignore (run sa sb);
  send sb (String.make 24 'n'); close sb;
  close sa;
  check_quiet "data+FIN crossing a FIN" (run sa sb);
  drain sa sb

(* both send data and FIN at the same time *)
let data_fin_both () =
  let sa, sb = establish () in
  send sa (String.make 24 'a'); close sa;
  send sb (String.make 24 'b'); close sb;
  check_quiet "data+FIN both ways" (run sa sb);
  drain sa sb

(* ordinary close, one side first *)
let sequential_close () =
  let sa, sb = establish () in
  send sb (String.make 24 'n'); close sb;
  ignore (run sa sb); recv sa; close sa;
  check_quiet "sequential close" (run sa sb);
  drain sa sb

let tests = [
  "simultaneous close without data", `Quick, simultaneous_close_no_data ;
  "simultaneous close after an exchange", `Quick, simultaneous_close_after_exchange ;
  "data+FIN crossing a FIN", `Quick, data_fin_crossing_fin ;
  "data+FIN in both directions", `Quick, data_fin_both ;
  "sequential close", `Quick, sequential_close ;
]
