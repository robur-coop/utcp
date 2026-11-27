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

let equal_tcp_full a b =
  State.IS.equal a.State.listeners b.State.listeners &&
  State.CM.equal equal_conn_state_full a.connections b.connections

let test_full_state = Alcotest.testable (State.pp Mtime.min_stamp) equal_tcp_full
and test_seg = Alcotest.testable Segment.pp Segment.equal
and test_ip =
  Alcotest.testable Ipaddr.pp (fun a b -> Ipaddr.compare a b = 0)

let test_full_handle =
  Alcotest.(pair test_full_state (list (triple test_ip test_ip test_seg)))

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
let test_segs ?(seq = initial_seq) ack payload =
  let seg = { basic_seg with seq }
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

(* 18 segments with 3 different flags set, each with and without ack (so 9), once before, in, and after window *)
let window_segs ?(seq = initial_seq) ack =
  (* TODO test with delayed ack in mind -- so where last_ack_sent and rcv_nxt is different! *)
  [
    "RST before window", { basic_seg with flag = Some `Rst };
    "RST+ACK before window", { basic_seg with flag = Some `Rst ; ack = Some ack };
    "RST in window", { basic_seg with flag = Some `Rst ; seq = Sequence.addi seq 1 };
    "RST+ACK in window", { basic_seg with flag = Some `Rst ; seq = Sequence.addi seq 1 ; ack = Some ack };
    "RST after window", { basic_seg with flag = Some `Rst ; seq = Sequence.addi seq 70000 };
    "RST+ACK after window", { basic_seg with flag = Some `Rst ; seq = Sequence.addi seq 70000 ; ack = Some ack };
    "SYN before window", { basic_seg with flag = Some `Syn };
    "SYN+ACK before window", { basic_seg with flag = Some `Syn; ack = Some ack };
    "SYN in window", { basic_seg with flag = Some `Syn ; seq = Sequence.addi seq 1 };
    "SYN+ACK in window", { basic_seg with flag = Some `Syn ; seq = Sequence.addi seq 1 ; ack = Some ack };
    "SYN after window", { basic_seg with flag = Some `Syn ; seq = Sequence.addi seq 70000 };
    "SYN+ACK after window", { basic_seg with flag = Some `Syn ; seq = Sequence.addi seq 70000 ; ack = Some ack };
    "FIN before window", { basic_seg with flag = Some `Fin };
    "FIN+ACK before window", { basic_seg with flag = Some `Fin ; ack = Some ack };
    "FIN in window", { basic_seg with flag = Some `Fin ; seq = Sequence.addi seq 1 };
    "FIN+ACK in window", { basic_seg with flag = Some `Fin ; seq = Sequence.addi seq 1 ; ack = Some ack };
    "FIN after window", { basic_seg with flag = Some `Fin ; seq = Sequence.addi seq 70000 };
    "FIN+ACK after window", { basic_seg with flag = Some `Fin ; seq = Sequence.addi seq 70000 ; ack = Some ack };
  ]

(* we encode the answers to each of these 8 segments for each state *)
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

let no_state_window =
  let rst = {
    basic_seg with src_port = listen_port ; dst_port = src_port ;
    flag = Some `Rst ; seq = initial_ack
  } in
  let rst_ack = { rst with seq = Sequence.zero ; ack = Some (Sequence.addi Sequence.zero 1) } in
  [
    (* RST *)
    None; None; None; None; None; None;
    (* SYN *)
    Some rst_ack; Some rst;
    Some { rst_ack with ack = Some (Sequence.addi initial_seq 2) }; Some rst;
    Some { rst_ack with ack = Some (Sequence.of_int32 70043l) }; Some rst;
    (* FIN *)
    Some rst_ack; Some rst;
    Some { rst_ack with ack = Some (Sequence.addi initial_seq 2) }; Some rst;
    Some { rst_ack with ack = Some (Sequence.of_int32 70043l) }; Some rst;
  ]

let test_closed_window =
  List.map2 (fun (name, seg) (cnt, reply) ->
      "window closed " ^ string_of_int cnt, `Quick,
      fun () ->
        Alcotest.check test_full_handle ("CLOSED, seg " ^ name) (tcp, reply)
          (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg))
    (window_segs initial_ack)
    (List.mapi (fun i v ->
         i, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
        no_state_window)

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
    let now = Mtime.of_uint64_ns 0L in
    let timer = Subr.start_tt_rexmt now 0 false State.initial_cb.t_rttinf in
    let cb = { State.initial_cb with snd_una = rng_seq ; snd_max = Sequence.incr rng_seq ; snd_nxt = Sequence.incr rng_seq ; iss = rng_seq ; snd_cwnd = 2144 ; rcv_wnd = 65928 ; rcv_nxt = Sequence.incr initial_seq ; irs = initial_seq ; rcv_adv = Sequence.of_int32 65971l ; last_ack_sent = Sequence.incr initial_seq ; t_advmss = 1460 ; tt_rexmt = timer ; t_rttseg = Some (now, rng_seq) } in
    let conn = State.conn_state Mtime.min_stamp (fun () -> ()) ~rcvbufsize:65928 ~sndbufsize:33232 Syn_received cb in
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
          Alcotest.check test_full_handle ("LISTEN, seg " ^ name) (tcp, reply)
            (Input.handle_segment tcp_listen (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs initial_ack p)
      (List.mapi (fun i (s, v) ->
           i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          listen)
  in
  test_all " " String.empty @ test_all "+data " (String.make 20 '\000')

let listen_window =
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
  let tcp' irs rcv_adv =
    let rcv_nxt = Sequence.incr irs in
    let now = Mtime.of_uint64_ns 0L in
    let timer = Subr.start_tt_rexmt now 0 false State.initial_cb.t_rttinf in
    let cb = { State.initial_cb with snd_una = rng_seq ; snd_max = Sequence.incr rng_seq ; snd_nxt = Sequence.incr rng_seq ; iss = rng_seq ; snd_cwnd = 2144 ; rcv_wnd = 65928 ; rcv_nxt ; irs ; rcv_adv ; last_ack_sent = rcv_nxt ; t_advmss = 1460 ; tt_rexmt = timer ; t_rttseg = Some (now, rng_seq) } in
    let conn = State.conn_state Mtime.min_stamp (fun () -> ()) ~rcvbufsize:65928 ~sndbufsize:33232 Syn_received cb in
    { tcp_listen with connections = State.CM.add quad conn tcp_listen.connections }
  in
  [ (* RST *)
    tcp_listen, None; tcp_listen, None; tcp_listen, None; tcp_listen, None; tcp_listen, None; tcp_listen, None;
    (* SYN *)
    tcp' (Sequence.of_int32 0l) (Sequence.of_int32 65929l), Some { synack with ack = Some (Sequence.of_int32 1l) };
    tcp_listen, Some rst;
    tcp' (Sequence.of_int32 43l) (Sequence.of_int32 65972l), Some { synack with ack = Some (Sequence.of_int32 44l) };
    tcp_listen, Some rst;
    tcp' (Sequence.of_int32 70042l) (Sequence.of_int32 135971l), Some { synack with ack = Some (Sequence.of_int32 70043l) };
    tcp_listen, Some rst;
    (* FIN *)
    tcp_listen, None; tcp_listen, Some rst;
    tcp_listen, None; tcp_listen, Some rst;
    tcp_listen, None; tcp_listen, Some rst; ]

let test_listen_window =
  List.map2 (fun (name, seg) (cnt, tcp, reply) ->
      "window listen " ^ string_of_int cnt, `Quick,
      fun () ->
        Alcotest.check test_full_handle ("LISTEN, seg " ^ name) (tcp, reply)
          (Input.handle_segment tcp_listen (Mtime.of_uint64_ns 0L) quad seg))
    (window_segs initial_ack)
    (List.mapi (fun i (s, v) ->
         i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
        listen_window)

let tcp_syn_sent =
  let tcp', _id, _signal, _out =
    Result.get_ok
      (User.connect ~src:my_ip ~src_port:listen_port ~dst:your_ip ~dst_port:src_port tcp (Mtime.of_uint64_ns 0L))
  in
  tcp'

let syn_sent_ack_iss, syn_sent_ack_bad =
  let tcp_est =
    let conn = State.CM.find quad tcp_syn_sent.connections in
    let cb = conn.control_block in
    let rttinf = { cb.t_rttinf with t_rttupdated = 1 ; tf_srtt_valid = true ; t_srtt = 0L ; t_rttvar = 0L ; t_lastrtt = Some 0L; t_lastshift =  Some 0 } in
    let cb = { cb with snd_una = Sequence.incr cb.snd_una ; snd_wl1 = Sequence.incr initial_seq ; snd_wl2 = Sequence.incr rng_seq ; snd_cwnd = 2144 ; rcv_nxt = Sequence.incr initial_seq ; irs = initial_seq ; rcv_adv = Sequence.of_int32 65578l ; last_ack_sent = Sequence.incr initial_seq ; t_rttseg = None ; tt_conn_est = None ; tt_rexmt = None ; t_rttinf = rttinf } in
    let conn' = { conn with tcp_state = Established ; control_block = cb ; rcvbufsize = 65928 ; sndbufsize = 33232 } in
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
        "syn_sent" ^ s ^ string_of_int cnt, `Quick,
        fun () ->
          Alcotest.check test_full_handle ("SYN_SENT, seg " ^ name) (tcp, reply)
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

let syn_sent_window =
  let tcp_est irs rcv_adv =
    let rcv_nxt = Sequence.incr irs in
    let conn = State.CM.find quad tcp_syn_sent.connections in
    let cb = conn.control_block in
    let rttinf = { cb.t_rttinf with t_rttupdated = 1 ; tf_srtt_valid = true ; t_srtt = 0L ; t_rttvar = 0L ; t_lastrtt = Some 0L; t_lastshift =  Some 0 } in
    let cb = { cb with snd_una = Sequence.incr cb.snd_una ; snd_wl1 = rcv_nxt ; snd_wl2 = Sequence.incr rng_seq ; snd_cwnd = 2144 ; rcv_nxt ; irs ; rcv_adv ; last_ack_sent = rcv_nxt ; t_rttseg = None ; tt_conn_est = None ; tt_rexmt = None ; t_rttinf = rttinf } in
    let conn' = { conn with tcp_state = Established ; control_block = cb ; rcvbufsize = 65928 ; sndbufsize = 33232 } in
    { tcp_syn_sent with connections = State.CM.add quad conn' tcp_syn_sent.connections }
  and ack = {
    basic_seg with src_port = listen_port ; dst_port = src_port ;
                   seq = Sequence.incr rng_seq ;
                   ack = Some (Sequence.incr initial_seq) ;
                   flag = None ;
                   window = 1 lsl 16 - 1 ;
  } in
  [ (* RST *)
    tcp_syn_sent, None; tcp, None;
    tcp_syn_sent, None; tcp, None;
    tcp_syn_sent, None; tcp, None;
    (* SYN *)
    tcp_syn_sent, None; tcp_est Sequence.zero (Sequence.of_int32 65536l), Some { ack with ack = Some (Sequence.of_int32 1l) };
    tcp_syn_sent, None; tcp_est (Sequence.of_int32 43l) (Sequence.of_int32 65579l), Some { ack with ack = Some (Sequence.of_int32 44l) };
    tcp_syn_sent, None; tcp_est (Sequence.of_int32 70042l) (Sequence.of_int32 135578l), Some { ack with ack = Some (Sequence.of_int32 70043l) };
    (* FIN *)
    tcp_syn_sent, None; tcp_syn_sent, None;
    tcp_syn_sent, None; tcp_syn_sent, None;
    tcp_syn_sent, None; tcp_syn_sent, None;
  ]

let test_syn_sent_window =
  List.map2 (fun (name, seg) (cnt, tcp, reply) ->
      "window syn_sent " ^ string_of_int cnt, `Quick,
      fun () ->
        Alcotest.check test_full_handle ("SYN_SENT, seg " ^ name) (tcp, reply)
          (Input.handle_segment tcp_syn_sent (Mtime.of_uint64_ns 0L) quad seg))
    (window_segs (Sequence.incr rng_seq))
    (List.mapi (fun i (s, v) ->
         i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
        syn_sent_window)

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
    let cb = conn.control_block in
    let cb = { cb with snd_una = Sequence.incr cb.snd_una ; snd_wl1 = initial_seq ; snd_wl2 = Sequence.incr rng_seq } in
    let conn' = { conn with tcp_state = Established ; control_block = cb } in
    { tcp_syn_rcvd with connections = State.CM.add quad conn' tcp_syn_rcvd.connections }
  in
  [
    tcp_syn_rcvd, None; (* atm - drop + AR *)
    tcp', None; (* ~> est *)
    tcp_syn_rcvd, None; (* comefrom listen ~> listen, otherwise (sim-open) challenge-ack -- atm AR *)
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
        fun () ->
          Alcotest.check test_full_handle ("SYN_RCVD, seg " ^ name) (tcp, reply)
            (Input.handle_segment tcp_syn_rcvd (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs ack p)
      (List.mapi (fun i (s, v) ->
           i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          syn_received)
  in
  test_all " " (Sequence.incr rng_seq) String.empty

let syn_rcvd_window = [
  (* RST *)
  tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None;
  (* SYN *)
  tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None;
  (* FIN *)
  tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None; tcp_syn_rcvd, None;
]

let test_syn_rcvd_window =
  List.map2 (fun (name, seg) (cnt, tcp, reply) ->
      "window syn received " ^ string_of_int cnt, `Quick,
        fun () ->
          Alcotest.check test_full_handle ("SYN_RCVD, seg " ^ name) (tcp, reply)
            (Input.handle_segment tcp_syn_rcvd (Mtime.of_uint64_ns 0L) quad seg))
      (window_segs (Sequence.incr rng_seq))
      (List.mapi (fun i (s, v) ->
           i, s, match v with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          syn_rcvd_window)

let to_established ~now =
  let tcp = State.start_listen tcp listen_port in
  let syn = { basic_seg with flag = Some `Syn ; seq = initial_seq } in
  (* SYN ([out] should be SYN+ACK) *)
  let tcp, _ = Input.handle_segment tcp now quad syn in
  let ack = { basic_seg with seq = Sequence.incr initial_seq ;
                             ack = Some (Sequence.incr rng_seq) ;
                             window = 1 lsl 16 - 1 } in
  (* ACK (we should be in the [Established] mode) *)
  let tcp, _ = Input.handle_segment tcp now quad ack in
  tcp

let test_established =
 let est = to_established ~now:(Mtime.of_uint64_ns 0L) in
  let cw (tcp : 'a State.t) len =
    let conn = State.CM.find quad tcp.connections in
    let cb = conn.control_block in
    let cb = { cb with rcv_wnd = cb.rcv_wnd - 1 - len ; rcv_nxt = Sequence.addi cb.rcv_nxt (1 + len) ; last_ack_sent = Sequence.addi cb.last_ack_sent (1 + len) } in
    let conn' = { conn with tcp_state = Close_wait ; cantrcvmore = true ; control_block = cb ; rcvq = Rope.of_string (String.make len '\000') } in
    { tcp with connections = State.CM.add quad conn' tcp.connections }
  in
  let tcp_with_p tcp len =
    let conn = State.CM.find quad tcp.State.connections in
    let cb = conn.control_block in
    let tt_delack =
      if len = 0 then None else Some (Timers.timer (Mtime.of_uint64_ns 0L) () (Duration.of_ms 100))
    in
    let cb = { cb with rcv_nxt = Sequence.addi cb.rcv_nxt len ; rcv_wnd = cb.rcv_wnd - len ; tt_delack } in
    let rcvq = Rope.of_string (String.make len '\000') in
    let conn' = { conn with control_block = cb; rcvq } in
    { tcp with connections = State.CM.add quad conn' tcp.connections }
  in
  let states tcp plen =
    [
      tcp, None ;
      tcp_with_p tcp plen, None ;
      tcp, Some { basic_seg with src_port = listen_port ; dst_port = src_port ; seq = Sequence.incr rng_seq ; ack = Some (Sequence.incr initial_seq); window = 65535 };
      tcp_listen, None ;
      tcp, None ;
      tcp, Some { basic_seg with src_port = listen_port ; dst_port = src_port ; seq = Sequence.incr rng_seq ; ack = Some (Sequence.incr initial_seq); window = 65535 } ;
      tcp_listen, None ;
      cw tcp plen, Some { basic_seg with src_port = listen_port ; dst_port = src_port ; seq = Sequence.incr rng_seq ; ack = Some (Sequence.addi initial_seq (2 + plen)); window = 65535 } ;
    ]
  in
  let no_payload =
    List.map2 (fun (name, seg) (cnt, tcp, reply) ->
        "established " ^ string_of_int cnt, `Quick, fun () ->
          Alcotest.check test_full_handle ("ESTABLISHED, seg " ^ name) (tcp, reply)
            (Input.handle_segment est (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs ~seq:(Sequence.incr initial_seq) (Sequence.incr rng_seq) "")
      (List.mapi (fun idx (tcp, seg) ->
           idx, tcp, match seg with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          (states est 0))
  in
  let payload =
    List.map2 (fun (name, seg) (cnt, tcp, reply) ->
        "data + established " ^ string_of_int cnt, `Quick, fun () ->
          Alcotest.check test_full_handle ("ESTABLISHED, seg " ^ name) (tcp, reply)
            (Input.handle_segment est (Mtime.of_uint64_ns 0L) quad seg))
      (test_segs ~seq:(Sequence.incr initial_seq) (Sequence.incr rng_seq) (String.make 20 '\000'))
      (List.mapi (fun idx (tcp, seg) ->
           idx, tcp, match seg with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
          (states est 20))
  in
  no_payload @ payload

let test_established_window =
  let est = to_established ~now:(Mtime.of_uint64_ns 0L) in
  let seg = { basic_seg with src_port = listen_port ; dst_port = src_port ; window = 65535 } in
  let states tcp =
    [
      tcp, None ;
      tcp, None ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, None ;
      tcp, None ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, None ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
      tcp, Some { seg with ack = Some (Sequence.incr initial_seq) ; seq = Sequence.incr rng_seq } ;
    ]
  in
  List.map2 (fun (name, seg) (cnt, tcp, reply) ->
      "window established " ^ string_of_int cnt, `Quick, fun () ->
        Alcotest.check test_full_handle ("ESTABLISHED, seg " ^ name) (tcp, reply)
          (Input.handle_segment est (Mtime.of_uint64_ns 0L) quad seg))
    (window_segs ~seq:(Sequence.incr initial_seq) (Sequence.incr initial_seq))
    (List.mapi (fun idx (tcp, seg) ->
         idx, tcp, match seg with None -> [] | Some s -> [ (my_ip, your_ip, s) ])
        (states est))

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
    let tcp, evs, _out = handle tcp _0ns ack in
    let id =
      List.fold_left (fun acc -> function
          | `Established (id, _) -> Some id
          | _ -> acc)
        None evs
    in
    let id = match id with
      | Some x -> x
      | None -> Alcotest.fail "expected an ESTABLISHED event"
    in
    let window = 1 lsl 16 - 1 in
    let data = { basic_seg with seq = Sequence.incr initial_seq ;
                                ack = Some (Sequence.incr rng_seq) ;
                                window = 1 lsl 16 - 1 ;
                                payload_len = window ;
                                payload = [ String.make window 'A' ] } in
    (* NOTE(dinosaure): [data] carries far more than 2 MSS (like a GRO segment):
       it must be ACKed at once, without waiting for the delayed ACK timer. This
       ACK tells us how much space is left in [rcvq]. *)
    let tcp, _ev, out = handle tcp _0ns data in
    let left = match out with
      | [ (_, _, seg) ] -> seg.Segment.window
      | _ -> Alcotest.fail "expected an immediate ACK for the data" in
    let _200ms = Mtime.of_uint64_ns (Duration.of_ms 200) in
    let tcp, drops, out = run_timers tcp _200ms 10 in
    Alcotest.(check int) "no connection is dropped" 0 (List.length drops);
    Alcotest.(check int) "no delayed ACK" 0 (List.length out);
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

let handle ~now tcp seg =
  let data = Segment.encode_and_checksum now ~src:your_ip ~dst:my_ip seg in
  handle_buf tcp now ~src:your_ip ~dst:my_ip data

let established ~now () =
  let ctr = ref 0 in
  let fetch_and_incr () = incr ctr; !ctr in
  let tcp = start_listen (empty fetch_and_incr "") listen_port in
  let syn = { basic_seg with flag = Some `Syn ; seq = initial_seq } in
  (* SYN ([out] should be SYN+ACK) *)
  let tcp, _ev, _out = handle ~now tcp syn in
  let ack = { basic_seg with seq = Sequence.incr initial_seq ;
                             ack = Some (Sequence.incr rng_seq) ;
                             window = 1 lsl 16 - 1 } in
  (* ACK (we should be in the [Established] mode) *)
  let tcp, evs, _out = handle ~now tcp ack in
  let id =
    List.fold_left (fun acc -> function
        | `Established (id, _) -> Some id
        | _ -> acc)
      None evs
  in
  let id =
    match id with
    | Some x -> x
    | None -> Alcotest.fail "expected an ESTABLISHED event"
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

let test_close_wait =
  let now = Mtime.of_uint64_ns 0L in
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
    let tcp, id, rcv_cond, _snd_cond = established ~now () in
    (* FIN ([out] should be ACK) *)
    let tcp, evs, _out = handle ~now tcp fin in
    let woken =
      List.fold_left (fun acc -> function
          | `Received (_, _, n) -> n :: acc
          | `Send (_, n) -> n :: acc
          | _ -> acc)
        [] evs
    in
    Alcotest.(check (list int)) "the reader is woken up" [ rcv_cond ] woken;
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
    let tcp, id, rcv_cond, snd_cond = established ~now () in
    let tcp, evs, _out = handle ~now tcp rst in
    let woken =
      List.fold_left (fun acc -> function
          | `Drop (_, conds) -> conds
          | _ -> acc)
        [] evs
    in
    Alcotest.(check (list int)) "all waiters are woken up with eof"
      [ rcv_cond ; snd_cond ] woken;
    match recv tcp now id with
    | Error `Not_found -> ()
    | _ -> Alcotest.fail "unexpected connection"
  in
  let ack_never_shrinks_snd_wnd_below_zero () =
    (* A shrunken advertised window followed by an ACK that covers more
       in-flight bytes than the current window must not drive snd_wnd
       negative: snd_wnd counts window space, so it floors at zero. *)
    let now = Mtime.of_uint64_ns 0L in
    let tcp0 = State.empty (fun () -> ()) "" in
    let isn = Sequence.of_int32 1000l in
    let snd_max = Sequence.addi isn 5000 in
    let cb = { State.initial_cb with
      State.snd_una = isn ;
      snd_max ;
      snd_nxt = snd_max ;
      snd_wnd = 1000 ;
      (* window update must not fire: pretend the last update came from a
         later segment, so the stale advertised window is kept *)
      snd_wl1 = Sequence.addi State.initial_cb.State.rcv_nxt 100 ;
      rcv_wnd = 1000 }
    in
    (* pretend we sent 5000 bytes (sndq tracks unacked data) *)
    let sndq = Rope.of_string (String.make 5000 'x') in
    let conn =
      State.conn_state now (fun () -> ())
        ~rcvbufsize:65535 ~sndbufsize:32768 State.Established cb
    in
    let conn = { conn with State.sndq } in
    let tcp0 =
      { tcp0 with State.connections = State.CM.add quad conn tcp0.State.connections }
    in
    (* ACK all 5000 bytes while the window stays shrunk at 1000 *)
    let seg = { basic_seg with seq = cb.State.rcv_nxt ;
                               ack = Some snd_max ; window = 1000 } in
    let tcp, _out = Input.handle_segment tcp0 now quad seg in
    let conn' = State.CM.find quad tcp.State.connections in
    Alcotest.(check int) "snd_wnd floored at zero" 0 conn'.State.control_block.State.snd_wnd
  in
  let fin_after_one_byte () =
    (* An echo server exchanging exactly one byte, then closing: snd_nxt is
       iss + 2 (SYN + 1 byte) when our FIN is first emitted. It must not be
       mistaken for a FIN retransmission (sent at snd_nxt - 1, on top of the
       data byte), otherwise the peer never acks it.

       C: SYN ->         ACK -> "p" ->           ACK+FIN ->       ACK ->
       S:     SYN+ACK ->              "p" ->               FIN ->
       s:                     [ESTABLISHED]       [CLOSE_WAIT] [LAST_ACK] [CLOSED] *)
    let tcp, id, _rcv_cond, _snd_cond = established ~now () in
    let data = { basic_seg with push = true ;
                                seq = Sequence.incr initial_seq ;
                                ack = Some (Sequence.incr rng_seq) ;
                                window = 1 lsl 16 - 1 ;
                                payload_len = 1 ; payload = [ "p" ] } in
    let tcp, _evs, _out = handle ~now tcp data in
    let tcp = match recv tcp now id with
      | Ok (tcp, bufs, _, _) ->
        Alcotest.(check string) "received one byte" "p" (String.concat "" bufs);
        tcp
      | Error _ -> Alcotest.fail "expected recv to deliver the byte"
    in
    let tcp = match send tcp now id "p" with
      | Ok (tcp, 1, _, _) -> tcp
      | _ -> Alcotest.fail "expected send to enqueue one byte"
    in
    let fin_ack = { basic_seg with flag = Some `Fin ;
                                   seq = Sequence.addi initial_seq 2 ;
                                   ack = Some (Sequence.addi rng_seq 2) ;
                                   window = 1 lsl 16 - 1 } in
    let tcp, _evs, _out = handle ~now tcp fin_ack in
    let tcp = match recv tcp now id with
      | Error `Eof -> tcp
      | _ -> Alcotest.fail "expected eof"
    in
    let tcp, outs = match close tcp now id with
      | Ok (tcp, _, outs) -> tcp, outs
      | Error _ -> Alcotest.fail "close failed"
    in
    let fins =
      List.filter_map (fun (_, _, seg) ->
          if seg.Segment.flag = Some `Fin then Some seg.Segment.seq else None)
        outs
    in
    Alcotest.(check (list sequence)) "FIN is sent after the data byte"
      [ Sequence.addi rng_seq 2 ] fins;
    let ack = { basic_seg with seq = Sequence.addi initial_seq 3 ;
                               ack = Some (Sequence.addi rng_seq 3) ;
                               window = 1 lsl 16 - 1 } in
    let tcp, _evs, _out = handle ~now tcp ack in
    Alcotest.(check int) "connection is closed once our FIN is acked"
      0 (num_connections tcp)
  in
  [ "FIN in Established signals, does not drop", `Quick, fin_on_established
  ; "RST in Established drops and wakes all waiters", `Quick, rst_on_established
  ; "ACK covering more than snd_wnd floors at zero", `Quick, ack_never_shrinks_snd_wnd_below_zero
  ; "FIN after exchanging exactly one byte", `Quick, fin_after_one_byte ]

let test_edge =
  let now = Mtime.of_uint64_ns 0L in
  let fin_at_the_right_edge_of_the_window () =
    (* NOTE(dinosaure): the client sends data which fill exactly our advertised
       window, with the FIN on the same segment. The FIN consumes one sequence
       number past the right edge of the window: once accepted, [rcv_nxt] is
       [rcv_adv + 1]. The window already advertised ([rcv_adv - rcv_nxt]) must
       be considered as 0 and not as [2^32 - 1] (an unsigned subtraction):
       otherwise, [rcv_adv] becomes [rcv_nxt - 1] and every following segment
       is unacceptable, even a pure ACK with [seq = rcv_nxt]. The ACK of our
       data is never processed, [snd_una] never advances and we retransmit
       forever.

       C: SYN ->            ACK ->                       data+FIN ->     ACK ->
       S:        SYN+ACK ->       data (window = w) ->               ACK
       s:                       | [ESTABLISHED]                     | [CLOSE_WAIT] *)
    let tcp, id, _rcv_cond, _snd_cond = established ~now () in
    (* the client sends some data first: our advertised window is then smaller
       than 65535 and not clamped, so it is exactly [rcv_adv - rcv_nxt] *)
    let first = 1000 in
    let data = { basic_seg with seq = Sequence.incr initial_seq ;
                                ack = Some (Sequence.incr rng_seq) ;
                                window = 1 lsl 16 - 1 ;
                                payload_len = first ;
                                payload = [ String.make first 'Z' ] } in
    let tcp, _evs, _out = handle ~now tcp data in
    (* the server sends some data, its segment advertises our window *)
    let tcp, window, snd_max = match send tcp now id "hello" with
      | Ok (tcp, 5, _, [ (_, _, seg) ]) ->
        tcp, seg.Segment.window, Sequence.addi seg.Segment.seq 5
      | Ok _ -> Alcotest.fail "expected one segment with our data"
      | Error _ -> Alcotest.fail "send failed"
    in
    (* the client fills exactly this window and closes *)
    let data_and_fin = { basic_seg with flag = Some `Fin ;
                                        seq = Sequence.addi initial_seq (1 + first) ;
                                        ack = Some (Sequence.incr rng_seq) ;
                                        window = 1 lsl 16 - 1 ;
                                        payload_len = window ;
                                        payload = [ String.make window 'A' ] } in
    let tcp, _evs, out = handle ~now tcp data_and_fin in
    let rcv_nxt = Sequence.addi initial_seq (first + window + 2) in
    begin match out with
      | [ (_, _, seg) ] ->
        Alcotest.(check (option sequence)) "data and FIN are acked"
          (Some rcv_nxt) seg.Segment.ack;
        (* with an unsigned subtraction, the window becomes 65535 (clamped) *)
        Alcotest.(check bool) "the advertised window does not grow" true
          (seg.Segment.window < window)
      | _ -> Alcotest.fail "expected an ACK for the data and the FIN" end;
    (* the client acks our data: this ACK must be acceptable (an unacceptable
       segment is answered with an ACK) *)
    let ack = { basic_seg with seq = rcv_nxt ; ack = Some snd_max ;
                               window = 1 lsl 16 - 1 } in
    let tcp, _evs, out = handle ~now tcp ack in
    Alcotest.(check int) "nothing to reply" 0 (List.length out);
    (* our data is acked: nothing is retransmitted once the RTO expires *)
    let _10s = Mtime.of_uint64_ns (Duration.of_sec 10) in
    let _tcp, drops, out = run_timers tcp _10s 10 in
    Alcotest.(check int) "no connection is dropped" 0 (List.length drops);
    let retransmitted =
      List.exists (fun (_, _, seg) -> seg.Segment.payload_len > 0) out in
    Alcotest.(check bool) "no retransmission" false retransmitted
  in
  [ "FIN at the right edge of the window keeps ACKs acceptable", `Quick,
    fin_at_the_right_edge_of_the_window ]

let tests =
  test_closed @ test_closed_window @
  test_listen @ test_listen_window @
  test_syn_sent @ test_syn_sent_window @
  test_syn_rcvd @ test_syn_rcvd_window @
  test_established @ test_established_window @
  test_window_probe @ test_close_wait @ test_edge
