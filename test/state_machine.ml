(* (c) 2019 Hannes Mehnert, all rights reserved *)

open Utcp

(* generates the bit pattern 0b10100101 *)
module Static_rng : Mirage_crypto_rng.Generator = struct
  type g = unit

  let block = 1

  let create ?time:_ () = ()

  let generate_into ~g:_ data ~off len =
    for i = off to off + len - 1 do
      Bytes.set data i '\xA5'
    done

  let reseed ~g:_ _bytes = ()

  let accumulate ~g:_ _source = `Acc (fun _data -> ())

  let seeded ~g:_ = true

  let pools = 1
end

let equal_rttinf a b =
  a.State.t_rttupdated = b.State.t_rttupdated &&
  a.tf_srtt_valid = b.tf_srtt_valid &&
  a.t_srtt = b.t_srtt &&
  a.t_rttvar = b.t_rttvar &&
  a.t_rttmin = b.t_rttmin &&
  (match a.t_lastrtt, b.t_lastrtt with
   | None, None -> true
   | Some a, Some b -> a = b
   | _ -> false) &&
  (match a.t_lastshift, b.t_lastshift with
   | None, None -> true
   | Some a, Some b -> a = b
   | _ -> false) &&
  a.t_wassyn = b.t_wassyn

let equal_timer_opt eq a b = match a, b with
  | None, None -> true
  | Some (a, d), Some (b, d') -> d = d' && eq a b
  | _ -> false

let equal_control_block a b =
  equal_timer_opt
    (fun (mode, shift) (mode', shift') -> mode = mode' && shift = shift')
    a.State.tt_rexmt b.State.tt_rexmt &&
  equal_timer_opt (fun () () -> true) a.tt_2msl b.tt_2msl &&
  equal_timer_opt (fun () () -> true) a.tt_delack b.tt_delack &&
  equal_timer_opt (fun () () -> true) a.tt_conn_est b.tt_conn_est &&
  equal_timer_opt (fun () () -> true) a.tt_fin_wait_2 b.tt_fin_wait_2 &&

  Mtime.equal a.t_idletime b.t_idletime &&
  a.tf_needfin = b.tf_needfin &&
  a.tf_shouldacknow = b.tf_shouldacknow &&

  Sequence.equal a.snd_una b.snd_una &&
  Sequence.equal a.snd_max b.snd_max &&
  Sequence.equal a.snd_nxt b.snd_nxt &&
  Sequence.equal a.snd_wl1 b.snd_wl1 &&
  Sequence.equal a.snd_wl2 b.snd_wl2 &&
  Sequence.equal a.iss b.iss &&
  a.snd_wnd = b.snd_wnd &&
  a.snd_cwnd = b.snd_cwnd &&
  a.snd_ssthresh = b.snd_ssthresh &&

  a.rcv_wnd = b.rcv_wnd &&
  a.tf_rxwin0sent = b.tf_rxwin0sent &&
  Sequence.equal a.rcv_nxt b.rcv_nxt &&
  Sequence.equal a.irs b.irs &&
  Sequence.equal a.rcv_adv b.rcv_adv &&
  Sequence.equal a.last_ack_sent b.last_ack_sent &&

  a.t_maxseg = b.t_maxseg &&
  a.t_advmss = b.t_advmss &&

  a.tf_doing_ws = b.tf_doing_ws &&
  (match a.request_r_scale, b.request_r_scale with
   | None, None -> true
   | Some a, Some b -> a = b
   | _ -> false) &&
  a.snd_scale = b.snd_scale &&
  a.rcv_scale = b.rcv_scale &&

  (match a.t_rttseg, b.t_rttseg with
   | None, None -> true
   | Some (ts, seq), Some (ts', seq') -> Mtime.equal ts ts' && Sequence.equal seq seq'
   | _ -> false) &&
  equal_rttinf a.t_rttinf b.t_rttinf &&

  a.t_dupacks = b.t_dupacks &&
  Mtime.equal a.t_badrxtwin b.t_badrxtwin &&
  a.snd_cwnd_prev = b.snd_cwnd_prev &&
  a.snd_ssthresh_prev = b.snd_ssthresh_prev &&
  Sequence.equal a.snd_recover b.snd_recover &&

  (match a.t_softerror, b.t_softerror with
   | None, None -> true
   | Some a, Some b -> String.equal a b
   | _ -> false)

let equal_tcp_state a b = match a, b with
  | State.Syn_sent, State.Syn_sent
  | Syn_received, Syn_received
  | Established, Established
  | Close_wait, Close_wait
  | Fin_wait_1, Fin_wait_1
  | Closing, Closing
  | Last_ack, Last_ack
  | Fin_wait_2, Fin_wait_2
  | Time_wait, Time_wait -> true
  | _ -> false

let leq f a b =
  List.length a = List.length b &&
  List.for_all2 f a b

let equal_conn_state_full a b =
  equal_tcp_state a.State.tcp_state b.State.tcp_state &&
  a.cantrcvmore = b.cantrcvmore &&
  a.cantsndmore = b.cantsndmore &&
  a.rcvbufsize = b.rcvbufsize &&
  a.sndbufsize = b.sndbufsize &&
  Rope.equal a.sndq b.sndq &&
  Rope.equal a.rcvq b.rcvq &&
  equal_control_block a.control_block b.control_block

let equal_conn_state a b =
  equal_tcp_state a.State.tcp_state b.State.tcp_state &&
  a.cantrcvmore = b.cantrcvmore &&
  a.cantsndmore = b.cantsndmore &&
  Rope.equal a.sndq b.sndq &&
  Rope.equal a.rcvq b.rcvq

let equal_tcp_full a b =
  State.IS.equal a.State.listeners b.State.listeners &&
  State.CM.equal equal_conn_state_full a.connections b.connections

let equal_tcp a b =
  State.IS.equal a.State.listeners b.State.listeners &&
  State.CM.equal equal_conn_state a.connections b.connections

let test_full_state = Alcotest.testable (State.pp Mtime.min_stamp) equal_tcp_full
and test_state = Alcotest.testable (State.pp Mtime.min_stamp) equal_tcp
and test_seg = Alcotest.testable Segment.pp Segment.equal
and test_ip =
  Alcotest.testable Ipaddr.pp (fun a b -> Ipaddr.compare a b = 0)

let test_full_handle =
  Alcotest.(pair test_full_state (list (triple test_ip test_ip test_seg)))
and test_handle =
  Alcotest.(pair test_state (list (triple test_ip test_ip test_seg)))

(* some setup for testing, they should not be relevant (famous last words) *)
let my_ip = Ipaddr.(V4 (V4.of_string_exn "1.2.3.4"))
and your_ip = Ipaddr.(V4 (V4.of_string_exn "1.2.3.5"))
and listen_port = 1234
and src_port = 4321

let quad = Obj.magic (my_ip, listen_port, your_ip, src_port)

(* a TCP stack listening on port 1234 *)
let () = Mirage_crypto_rng.set_default_generator (Mirage_crypto_rng.create (module Static_rng))
let tcp = State.empty Fun.id ""
let tcp_listen = State.start_listen tcp listen_port

let basic_seg = {
  Segment.src_port ; dst_port = listen_port ; seq = Sequence.zero ;
  ack = None ; flag = None ; push = false ; window = 0 ;
  options = [] ; payload_len = 0 ; payload = [ String.empty ]
}

let initial_seq = Sequence.of_int32 42l
and initial_ack = Sequence.of_int32 23l
and rng_seq = Sequence.of_int32 0xA5A5A5A5l
let sequence = Alcotest.testable Sequence.pp Sequence.equal

(* 8 different input segments for testing our state machine *)
let test_segs ack payload =
  let seg = { basic_seg with seq = initial_seq }
  and str =
    let l = String.length payload in
    if l = 0 then "" else "+" ^ string_of_int l ^ " bytes data"
  and payload_len = String.length payload in
  let payload = [ payload ] in
  [
    "NONE" ^ str, { seg with payload_len ; payload } ;
    "ACK" ^ str, { seg with ack = Some ack ; payload_len ; payload } ;
    "SYN" ^ str, { seg with flag = Some `Syn ; payload_len ; payload } ;
    "RST" ^ str, { seg with flag = Some `Rst ; payload_len ; payload } ;
    "FIN" ^ str, { seg with flag = Some `Fin ; payload_len ; payload } ;
    "SYN+ACK" ^ str, { seg with flag = Some `Syn ; ack = Some ack ; payload_len ; payload } ;
    "RST+ACK" ^ str, { seg with flag = Some `Rst ; ack = Some ack ; payload_len ; payload } ;
    "FIN+ACK" ^ str, { seg with flag = Some `Fin ; ack = Some ack ; payload_len ; payload } ;
  ]

(* we encode the answers to each of these 16 segments for each state *)
let no_state dl =
  let rst = {
    basic_seg with src_port = listen_port ; dst_port = src_port ;
    flag = Some `Rst ; seq = initial_ack
  } in
  let rst_ack = { rst with ack = Some initial_seq ; seq = Sequence.zero } in
  let with_ack f s =
    let ack = match s.Segment.ack with None -> assert false | Some a -> Some (f a) in
    { s with ack }
  in
  [
    Some (with_ack (fun a -> Sequence.addi a dl) rst_ack) ;
    Some rst ;
    Some (with_ack (fun a -> Sequence.(incr (addi a dl))) rst_ack) ;
    None ;
    Some (with_ack (fun a -> Sequence.(incr (addi a dl))) rst_ack) ;
    Some rst ;
    None ;
    Some rst ;
  ]

let test_closed =
  let test_all s p =
    List.map2 (fun (name, seg) (cnt, reply) ->
        "closed" ^ s ^ string_of_int cnt, `Quick,
        fun () ->
          Alcotest.check test_full_handle ("CLOSED, seg " ^ name) (tcp, reply)
            (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs initial_ack p)
      (List.mapi (fun i v ->
           i, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          (no_state (String.length p)))
  in
  test_all " " String.empty @ test_all "+data " (String.make 20 '\000')

let listen =
  let rst = {
    basic_seg with src_port = listen_port ; dst_port = src_port ;
                   seq = initial_ack ; ack = None ;
                   flag = Some `Rst ;
  } in
  let synack = {
    rst with seq = rng_seq ; ack = Some (Sequence.incr initial_seq) ;
             flag = Some `Syn ;
             window = 1 lsl 16 - 1 ;
             options = [ Segment.MaximumSegmentSize 1460 ] }
  in
  let tcp' =
    let conn = State.conn_state Mtime.min_stamp (fun () -> ()) ~rcvbufsize:0 ~sndbufsize:0 Syn_received State.initial_cb in
    { tcp_listen with connections = State.CM.add quad conn tcp_listen.connections }
  in [
    tcp_listen, None ;
    tcp_listen, Some rst ;
    tcp', Some synack ;
    tcp_listen, None ;
    tcp_listen, None ;
    tcp_listen, Some rst ;
    tcp_listen, None ;
    tcp_listen, Some rst ;
  ]

(* TODO we should test various TCP options in the SYN (and test cb)! *)
let test_listen =
  let test_all s p =
    List.map2 (fun (name, seg) (cnt, tcp, reply) ->
        "listen" ^ s ^ string_of_int cnt, `Quick,
        fun () ->
          Alcotest.check test_handle ("LISTEN, seg " ^ name) (tcp, reply)
            (Input.handle_segment tcp_listen (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs initial_ack p)
      (List.mapi (fun i (s, v) ->
           i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          listen)
  in
  test_all " " String.empty @ test_all "+data " (String.make 20 '\000')

let tcp_syn_sent =
  let tcp', _id, _signal, _out =
    User.connect ~src:my_ip ~src_port:listen_port ~dst:your_ip ~dst_port:src_port tcp (Mtime.of_uint64_ns 0L)
  in
  tcp'

let syn_sent_ack_iss, syn_sent_ack_bad =
  let tcp_est =
    let conn = State.CM.find quad tcp_syn_sent.connections in
    let conn' = { conn with tcp_state = Established } in
    { tcp_syn_sent with connections = State.CM.add quad conn' tcp_syn_sent.connections }
  and ack = {
    basic_seg with src_port = listen_port ; dst_port = src_port ;
                   seq = Sequence.incr rng_seq ;
                   ack = Some (Sequence.incr initial_seq) ;
                   flag = None ;
                   window = 1 lsl 16 - 1 ;
  } in [
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_syn_sent, None ; (* this is simultaneous open, which we skip atm *)
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_est, Some ack ;
    tcp, None ;
    tcp_syn_sent, None ;
  ], [
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
    tcp_syn_sent, None ;
  ]

let test_syn_sent =
  let test_all s ack p res =
    List.map2 (fun (name, seg) (cnt, tcp, reply) ->
        if cnt = 2 then
          "skip simultaneous open", `Quick, fun () -> ()
        else
          "syn_sent" ^ s ^ string_of_int cnt, `Quick,
          fun () ->
            Alcotest.check test_handle ("SYN_SENT, seg " ^ name) (tcp, reply)
              (Input.handle_segment tcp_syn_sent (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs ack p)
      (List.mapi (fun i (s, v) ->
           i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          res)
  in
  test_all " " (Sequence.incr rng_seq) String.empty syn_sent_ack_iss @
  test_all "+iss " rng_seq String.empty syn_sent_ack_bad @
  test_all "+iss+2 " (Sequence.addi rng_seq 2) String.empty syn_sent_ack_bad @
  test_all "+data " (Sequence.incr rng_seq) (String.make 20 '\000') syn_sent_ack_iss @
  test_all "+data+iss " rng_seq (String.make 20 '\000') syn_sent_ack_bad @
  test_all "+data+iss+2 " (Sequence.addi rng_seq 2) (String.make 20 '\000') syn_sent_ack_bad

let tcp_syn_rcvd =
  let syn = { basic_seg with
              flag = Some `Syn ;
              seq = Sequence.addi initial_seq (-1) }
  in
  fst (Input.handle_segment tcp_listen (Mtime.of_uint64_ns 0L) quad syn)
(* now, the syn lead to a new connection, and a syn-ack being transmitted *)
(* anything apart from an ACK segment should be dropped with reset *)

let syn_received =
  let tcp' =
    let conn = State.CM.find quad tcp_syn_rcvd.connections in
    let conn' = { conn with tcp_state = Established } in
    { tcp_syn_rcvd with connections = State.CM.add quad conn' tcp_syn_rcvd.connections }
  in
  [
    tcp_syn_rcvd, None; (* atm - drop + AR *)
    tcp', None; (* ~> est *)
    tcp_listen, None; (* comefrom listen ~> listen, otherwise (sim-open) challenge-ack -- atm AR *)
    tcp_listen, None;
    tcp_syn_rcvd, None; (* atm drop + AR [should just drop, no ACK] *)
    tcp_syn_rcvd, None; (* sim open (when seq--) *)
    tcp_listen, None;
    tcp_syn_rcvd, None; (* ~> close_wait *)
  ]

(* we already know irs ~> need to check in addition to the flag combinations
   also for segments being in-window [left & right edge], and out of window *)
let test_syn_rcvd =
  let test_all s ack p =
    List.map2 (fun (name, seg) (cnt, tcp, reply) ->
        "syn received" ^ s ^ string_of_int cnt, `Quick,
        if cnt = 0 || cnt = 2 then fun () -> () else
        fun () ->
          Alcotest.check test_handle ("SYN_RCVD, seg " ^ name) (tcp, reply)
            (Input.handle_segment tcp_syn_rcvd (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs ack p)
      (List.mapi (fun i (s, v) ->
           i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          syn_received)
  in
  test_all " " (Sequence.incr rng_seq) String.empty

let run_timers tcp now n =
  let rec go tcp drops outs = function
    | 0 -> tcp, List.rev drops, List.rev outs
    | n ->
      let tcp, drops', outs' = timer tcp now in
      go tcp (List.rev_append drops' drops) (List.rev_append outs' outs)
        (pred n)
  in
  go tcp [] [] n

let test_window_probe =
  (* NOTE(dinosaure): here, we are the receiver and our receive buffer fills
     up ([rcv_wnd] = 0, it becomes full). The peer is in persist mode and sends
     zero-window probes (one byte past our window, thus unacceptable). Such a
     probe must be answered with an immediate ACK: it is the only way for the
     client to discover that our window has reopened once the user drained
     [rcvq] (the window update sent by [recv] can get lost). Dropping the probe
     silently can lead to a deadlock: the server must send a window update
     packet but this one can be lost (for whatever reason). If this packet is
     lost and if we drop window probe packets, the client will never know when
     the window has reopened and it will never retry to send back bytes. On the
     other side, the receiver will always wait bytes from the client. It's a
     deadlock!

     Here, we simulate such situation and expect an ACK when we send a
     window-probe packet. We also ensure that RST packets are still dropped (the
     initial behavior) in order to prevent the "blind-reset" attack (see RFC
     5961 § 3.2). *)
  let handle tcp now seg =
    let data = Segment.encode_and_checksum now ~src:your_ip ~dst:my_ip seg in
    handle_buf tcp now ~src:your_ip ~dst:my_ip data
  in
  let _0ns = Mtime.of_uint64_ns 0L in
  let established_and_filled () =
    let tcp = start_listen (empty (Fun.const ()) "") listen_port in
    let syn = { basic_seg with flag = Some `Syn ; seq = initial_seq } in
    (* SYN ([out] should be SYN+ACK) *)
    let tcp, _ev, _out = handle tcp _0ns syn in
    let ack = { basic_seg with seq = Sequence.incr initial_seq ;
                               ack = Some (Sequence.incr rng_seq) ;
                               window = 1 lsl 16 - 1 } in
    (* ACK (we should be in the [Established] mode) *)
    let tcp, ev, _out = handle tcp _0ns ack in
    let id = match ev with
      | Some (`Established (id, _)) -> id
      | _ -> Alcotest.fail "expected an ESTABLISHED event"
    in
    let window = 1 lsl 16 - 1 in
    let data = { basic_seg with seq = Sequence.incr initial_seq ;
                                ack = Some (Sequence.incr rng_seq) ;
                                window = 1 lsl 16 - 1 ;
                                payload_len = window ;
                                payload = [ String.make window 'A' ] } in
    let tcp, _ev, _out = handle tcp _0ns data in
    (* NOTE(dinosaure): the delayed ACK tells us how much space is left in
       [rcvq]. We can retrieve it after 100ms (we chosen 200ms), see
       [Params.tcptv_delack]. We would like to trigger [fast_timer] AND
       [slow_timer], so we iterate [run_timers] with [10]:
       - which is a multiple of [2]
       - and a multiple of [5] *)
    let _200ms = Mtime.of_uint64_ns (Duration.of_ms 200) in
    let tcp, drops, out = run_timers tcp _200ms 10 in
    Alcotest.(check int) "no connection is dropped" 0 (List.length drops);
    let left = match out with
      | [ (_, _, seg) ] -> seg.Segment.window
      | _ -> Alcotest.fail "expected a (delayed) ACK for the data"
    in
    (* fill the rest of the receive buffer: [rcv_wnd] falls to zero *)
    let tcp = match left with
      | 0 -> tcp
      | left ->
        let data = { data with seq = Sequence.addi initial_seq (1 + window) ;
                               payload_len = left ;
                               payload = [ String.make left 'B' ] } in
        let tcp, _ev, _out = handle tcp _200ms data in
        tcp
    in
    (* NOTE(dinosaure): calculate our next sequence number for our probe *)
    let filled = window + left in
    let probe_seq = Sequence.addi initial_seq (1 + filled) in
    tcp, id, probe_seq, filled
  in
  let probe seq = { basic_seg with seq ;
                                   ack = Some (Sequence.incr rng_seq) ;
                                   window = 1 lsl 16 - 1 ;
                                   payload_len = 1 ; payload = [ "X" ] } in
  let _400ms = Mtime.of_uint64_ns (Duration.of_ms 400) in
  let probe_is_acked () =
    let tcp, id, probe_seq, filled = established_and_filled () in
    (* first probe *)
    let tcp, _ev, out = handle tcp _400ms (probe probe_seq) in
    begin match out with
     | [ (_, _, seg) ] ->
       Alcotest.(check bool) "ACK"
         true (seg.Segment.ack <> None && seg.Segment.flag = None);
       Alcotest.(check int) "window-zero"
         0 seg.Segment.window;
       Alcotest.(check (option sequence)) "unacceptable probe"
         (Some probe_seq) seg.Segment.ack;
     | _ -> Alcotest.fail "expected ACK" end;
    (* read everything *)
    let tcp = match recv tcp _400ms id with
      | Ok (tcp, bufs, _, _out) ->
        Alcotest.(check int) "rcvq" filled
          (String.length (String.concat "" bufs));
        tcp
      | Error _ -> Alcotest.fail "no pending data"
    in
    (* second probe *)
    let tcp, _ev, _out = handle tcp _400ms (probe probe_seq) in
    match recv tcp _400ms id with
    | Ok (_, bufs, _, _) ->
      Alcotest.(check string) "probe" "X" (String.concat "" bufs)
    | Error _ -> Alcotest.fail "probe dropped"
  in
  let rst_out_of_window_is_dropped () =
    let tcp, id, probe_seq, _filled = established_and_filled () in
    (* RST packet *)
    let rst = { basic_seg with flag = Some `Rst ;
                               seq = Sequence.addi probe_seq 4242 } in
    let tcp, _ev, out = handle tcp _400ms rst in
    Alcotest.(check int) "no reply" 0 (List.length out);
    match recv tcp _400ms id with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "connection deleted"
  in
  [ "zero-window probe is acked", `Quick,
    probe_is_acked
  ; "out-of-window RST is dropped", `Quick,
    rst_out_of_window_is_dropped ]

let test_close_wait =
  let now = Mtime.of_uint64_ns 0L in
  let handle tcp seg =
    let data = Segment.encode_and_checksum now ~src:your_ip ~dst:my_ip seg in
    handle_buf tcp now ~src:your_ip ~dst:my_ip data
  in
  let established () =
    let ctr = ref 0 in
    let fetch_and_incr () = incr ctr; !ctr in
    let tcp = start_listen (empty fetch_and_incr "") listen_port in
    let syn = { basic_seg with flag = Some `Syn ; seq = initial_seq } in
    (* SYN ([out] should be SYN+ACK) *)
    let tcp, _ev, _out = handle tcp syn in
    let ack = { basic_seg with seq = Sequence.incr initial_seq ;
                               ack = Some (Sequence.incr rng_seq) ;
                               window = 1 lsl 16 - 1 } in
    (* ACK (we should be in the [Established] mode) *)
    let tcp, ev, _out = handle tcp ack in
    let id = match ev with
      | Some (`Established (id, None)) -> id
      | _ -> Alcotest.fail "expected an ESTABLISHED event"
    in
    let tcp, rcv_cond = match recv tcp now id with
      | Ok (tcp, _, cond, _) -> tcp, cond
      | Error _ -> Alcotest.fail "recv failed"
    in
    let tcp, snd_cond = match send tcp now id "" with
      | Ok (tcp, _, cond, _) -> tcp, cond
      | Error _ -> Alcotest.fail "send failed"
    in
    tcp, id, rcv_cond, snd_cond
  in
  let fin = { basic_seg with flag = Some `Fin ; push = true ;
                             seq = Sequence.incr initial_seq ;
                             ack = Some (Sequence.incr rng_seq) ;
                             window = 1 lsl 16 - 1 ;
                             payload_len = 4 ; payload = [ "bye!" ] }
  and rst = { basic_seg with flag = Some `Rst ;
                             seq = Sequence.incr initial_seq }
  in
  let fin_on_established () =
    (* NOTE(dinosaure): the aim here is to demonstrate that a client can
       'half-close' a connection. In other words, the client signals that it
       will no longer send any bytes, but the server can still send data. We
       must also ensure that the 'conditions' are properly released.

       [C]: Client
       [S]: Server
       [s]: state of the server

       C: SYN ->            ACK -> FIN ->
       S:        SYN+ACK ->     |               ACK ->
       s:                       | [ESTABLISHED]     | [CLOSE_WAIT] *)
    let tcp, id, rcv_cond, snd_cond = established () in
    (* FIN ([out] should be ACK) *)
    let tcp, ev, _out = handle tcp fin in
    begin match ev with
    | Some (`Signal (_, conds)) ->
      Alcotest.(check (list int)) "the reader (and writer) is woken up"
        [ rcv_cond ; snd_cond ] conds
    | Some (`Drop _) -> Alcotest.fail "unexpected drop"
    | _ -> Alcotest.fail "expected a signal" end;
    (* NOTE(dinosaure: The reader drains the pending data, and must observes
       [Eof] then. *)
    let tcp = match recv tcp now id with
      | Ok (tcp, bufs, _, _) ->
        Alcotest.(check string) "bye!"
          "bye!" (String.concat "" bufs) ;
        tcp
      | Error _ -> Alcotest.fail "expected recv to deliver the pending data"
    in
    let tcp = match recv tcp now id with
      | Error `Eof -> tcp
      | Ok _ -> Alcotest.fail "expected eof"
      | Error _ -> Alcotest.fail "expected eof"
    in
    (* NOTE(dinosaure): half-close: the connection is still usable for sending
       bytes by the server. *)
    match send tcp now id "ok" with
    | Ok (_, 2, _, _) -> ()
    | Ok _ -> Alcotest.fail "impossible to send data in CLOSE_WAIT"
    | Error _ -> Alcotest.fail "impossible to send data in CLOSE_WAIT" in
  let rst_on_established () =
    (* NOTE(dinosaure): the aim here is to show that we are correctly consuming
       our conditions despite an [RST] being sent by the client. *)
    let tcp, id, rcv_cond, snd_cond = established () in
    let tcp, ev, _out = handle tcp rst in
    begin match ev with
    | Some (`Drop (_, None, conds)) ->
      Alcotest.(check (list int)) "all waiters are woken up with eof"
        [ rcv_cond ; snd_cond ] conds
    | Some (`Drop (_, Some _, _)) ->
      Alcotest.fail "we don't have any readers"
    | _ -> Alcotest.fail "expected a drop event on RST reception" end;
    match recv tcp now id with
    | Error `Not_found -> ()
    | _ -> Alcotest.fail "unexpected connection" in
  [ "FIN in Established signals, does not drop", `Quick, fin_on_established
  ; "RST in Established drops and wakes all waiters", `Quick, rst_on_established ]

let tests =
  test_closed @ test_listen @ test_syn_sent @ test_syn_rcvd @ test_window_probe @ test_close_wait
