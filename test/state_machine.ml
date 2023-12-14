(* (c) 2019 Hannes Mehnert, all rights reserved *)

open Utcp

(* generates the bit pattern 0b10100101 *)
let static_rng y =
  let b = Cstruct.create y in Cstruct.memset b 0xA5 ; b

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
  leq Cstruct.equal a.sndq b.sndq &&
  leq Cstruct.equal a.rcvq b.rcvq &&
  equal_control_block a.control_block b.control_block

let equal_conn_state a b =
  equal_tcp_state a.State.tcp_state b.State.tcp_state &&
  a.cantrcvmore = b.cantrcvmore &&
  a.cantsndmore = b.cantsndmore &&
  leq Cstruct.equal a.sndq b.sndq &&
  leq Cstruct.equal a.rcvq b.rcvq

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
let tcp = State.empty Fun.id "" static_rng
let tcp_listen = State.start_listen tcp listen_port

let basic_seg = {
  Segment.src_port ; dst_port = listen_port ; seq = Sequence.zero ;
  ack = None ; flag = None ; push = false ; window = 0 ;
  options = [] ; payload = Cstruct.empty
}

let initial_seq = Sequence.of_int32 42l
and initial_ack = Sequence.of_int32 23l
and rng_seq = Sequence.of_int32 0xA5A5A5A5l

(* 8 different input segments for testing our state machine *)
let test_segs ack payload =
  let seg = { basic_seg with seq = initial_seq }
  and str =
    let l = Cstruct.length payload in
    if l = 0 then "" else "+" ^ string_of_int l ^ " bytes data"
  in [
    "NONE" ^ str, { seg with payload } ;
    "ACK" ^ str, { seg with ack = Some ack ; payload } ;
    "SYN" ^ str, { seg with flag = Some `Syn ; payload } ;
    "RST" ^ str, { seg with flag = Some `Rst ; payload } ;
    "FIN" ^ str, { seg with flag = Some `Fin ; payload } ;
    "SYN+ACK" ^ str, { seg with flag = Some `Syn ; ack = Some ack ; payload } ;
    "RST+ACK" ^ str, { seg with flag = Some `Rst ; ack = Some ack ; payload } ;
    "FIN+ACK" ^ str, { seg with flag = Some `Fin ; ack = Some ack ; payload } ;
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
          (no_state (Cstruct.length p)))
  in
  test_all " " Cstruct.empty @ test_all "+data " (Cstruct.create 20)

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
    let conn = State.conn_state (fun () -> ()) ~rcvbufsize:0 ~sndbufsize:0 Syn_received State.initial_cb in
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
  test_all " " Cstruct.empty @ test_all "+data " (Cstruct.create 20)

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
  test_all " " (Sequence.incr rng_seq) Cstruct.empty syn_sent_ack_iss @
  test_all "+iss " rng_seq Cstruct.empty syn_sent_ack_bad @
  test_all "+iss+2 " (Sequence.addi rng_seq 2) Cstruct.empty syn_sent_ack_bad @
  test_all "+data " (Sequence.incr rng_seq) (Cstruct.create 20) syn_sent_ack_iss @
  test_all "+data+iss " rng_seq (Cstruct.create 20) syn_sent_ack_bad @
  test_all "+data+iss+2 " (Sequence.addi rng_seq 2) (Cstruct.create 20) syn_sent_ack_bad

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
  test_all " " (Sequence.incr rng_seq) Cstruct.empty

let tests =
  test_closed @ test_listen @ test_syn_sent @ test_syn_rcvd
