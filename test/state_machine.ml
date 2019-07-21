(* (c) 2019 Hannes Mehnert, all rights reserved *)

open Tcp

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

let equal_conn_state_full a b =
  equal_tcp_state a.State.tcp_state b.State.tcp_state &&
  a.cantrcvmore = b.cantrcvmore &&
  a.cantsndmore = b.cantsndmore &&
  a.rcvbufsize = b.rcvbufsize &&
  a.sndbufsize = b.sndbufsize &&
  Cstruct.equal a.sndq b.sndq &&
  Cstruct.equal a.rcvq b.rcvq &&
  equal_control_block a.control_block b.control_block

let equal_conn_state a b =
  equal_tcp_state a.State.tcp_state b.State.tcp_state &&
  a.cantrcvmore = b.cantrcvmore &&
  a.cantsndmore = b.cantsndmore &&
  Cstruct.equal a.sndq b.sndq &&
  Cstruct.equal a.rcvq b.rcvq

let equal_tcp_full a b =
  Ipaddr.V4.compare a.State.ip b.State.ip = 0 &&
  State.IS.equal a.listeners b.listeners &&
  State.CM.equal equal_conn_state_full a.connections b.connections

let equal_tcp a b =
  Ipaddr.V4.compare a.State.ip b.State.ip = 0 &&
  State.IS.equal a.listeners b.listeners &&
  State.CM.equal equal_conn_state a.connections b.connections

let test_full_state = Alcotest.testable State.pp equal_tcp_full
and test_state = Alcotest.testable State.pp equal_tcp
and test_seg = Alcotest.testable Segment.pp Segment.equal
and test_ip =
  Alcotest.testable Ipaddr.V4.pp (fun a b -> Ipaddr.V4.compare a b = 0)

let test_full_handle =
  Alcotest.(pair test_full_state (option (pair test_ip test_seg)))
and test_handle =
  Alcotest.(pair test_state (option (pair test_ip test_seg)))

(* some setup for testing, they should not be relevant (famous last words) *)
let my_ip = Ipaddr.V4.of_string_exn "1.2.3.4"
and your_ip = Ipaddr.V4.of_string_exn "1.2.3.5"
and listen_port = 1234
and src_port = 4321

let quad = my_ip, listen_port, your_ip, src_port

(* a TCP stack listening on port 1234 *)
let tcp = State.start_listen (State.empty static_rng my_ip) listen_port

let basic_seg = {
  Segment.src_port ; dst_port = listen_port ; seq = Tcp.Sequence.zero ;
  ack = Sequence.zero ; flags = Segment.Flags.empty ; window = 0 ;
  options = [] ; payload = Cstruct.empty
}

(* now we need to put the stack into a TCP state, and push various segments,
   compare whether the return value is what we expect *)
let no_state () =
  (* when the TCP stack does not know the quadruple at all [and there is no
     listener], we expect either drop or RST (never RST a RST) *)
  (* what we test: NONE, ACK, SYN, RST, FIN, SYN+FIN, SYN+data *)
  (* what we could: SYN+ACK, FIN+ACK, RST+data, FIN+data, SYN+ACK+data, FIN+ACK+data *)
  let dst_port = 1235 in
  let quad = my_ip, dst_port, your_ip, src_port in
  let seg = { basic_seg with dst_port ; seq = Sequence.of_int32 42l } in
  let rst = { basic_seg with
              src_port = seg.dst_port ; dst_port = seg.src_port ;
              flags = Segment.Flags.of_list [`ACK ; `RST ] ; ack = seg.seq } in
  Alcotest.(check test_full_handle __LOC__ (tcp, Some (your_ip, rst))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg)) ;
  let seg' = { seg with flags = Segment.Flags.singleton `ACK ; ack = Sequence.of_int32 23l } in
  let rst' = { rst with flags = Segment.Flags.singleton `RST ; ack = Sequence.zero ; seq = seg'.ack } in
  Alcotest.(check test_full_handle __LOC__ (tcp, Some (your_ip, rst'))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg')) ;
  let seg'' = { seg with flags = Segment.Flags.singleton `SYN } in
  let rst'' = { rst with ack = Sequence.incr rst.ack } in
  Alcotest.(check test_full_handle __LOC__ (tcp, Some (your_ip, rst''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg'')) ;
  let seg''' = { seg with flags = Segment.Flags.singleton `RST } in
  Alcotest.(check test_full_handle __LOC__ (tcp, None)
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''')) ;
  let seg'''' = { seg with flags = Segment.Flags.singleton `FIN } in
  let rst''' = { rst with ack = Sequence.incr rst.ack } in
  Alcotest.(check test_full_handle __LOC__ (tcp, Some (your_ip, rst'''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg'''')) ;
  let seg''''' = { seg with flags = Segment.Flags.of_list [ `SYN ; `FIN ] } in
  let rst'''' = { rst with ack = Sequence.(incr (incr rst.ack)) } in
  Alcotest.(check test_full_handle __LOC__ (tcp, Some (your_ip, rst''''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''''')) ;
  let seg'''''' = { seg with flags = Segment.Flags.singleton `SYN ; payload = Cstruct.create 100 } in
  let rst''''' = { rst with ack = Sequence.(addi rst.ack 101) } in
  Alcotest.(check test_full_handle __LOC__ (tcp, Some (your_ip, rst'''''))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''''''))

let listen () =
  (* for a listen socket, anything apart from SYN should be RST (well, not RST) *)
  (* a SYN should lead to a new pcb in Syn_received state *)
  (* what we test: NONE, ACK, SYN, RST, FIN, SYN+FIN, SYN+data *)
  (* what we could: SYN+ACK, FIN+ACK, RST+data, FIN+data, SYN+ACK+data, FIN+ACK+data *)
  (* in addition, we should test various options *)
  let seg = { basic_seg with seq = Sequence.of_int32 42l } in
  Alcotest.(check test_full_handle __LOC__ (tcp, None)
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg)) ;
  let seg' = { seg with flags = Segment.Flags.singleton `ACK ; ack = Sequence.of_int32 23l } in
  let rst = { basic_seg with
               src_port = seg.dst_port ; dst_port = seg.src_port ;
               seq = seg'.ack ; ack = Sequence.zero ;
               flags = Segment.Flags.singleton `RST ;
            }
  in
  Alcotest.(check test_full_handle __LOC__ (tcp, Some (your_ip, rst))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg')) ;
  let seg'' = { seg with flags = Segment.Flags.singleton `SYN } in
  let seq = Sequence.of_int32 0xA5A5A5A5l in
  let ans = { rst with ack = Sequence.incr seg''.seq ; seq ;
                       flags = Segment.Flags.of_list [ `SYN ; `ACK ] ;
                       window = 1 lsl 16 - 1 ; options = [ Segment.MaximumSegmentSize 1460 ] }
  in
  let tcp' =
    let conn =
      State.conn_state ~rcvbufsize:Params.so_rcvbuf
        ~sndbufsize:Params.so_sndbuf Syn_received State.initial_cb
    in
    { tcp with connections = State.CM.add quad conn tcp.connections }
  in
  Alcotest.(check test_handle __LOC__
              (tcp', Some (your_ip, ans))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg'')) ;
  let seg''' = { seg with flags = Segment.Flags.singleton `RST } in
  Alcotest.(check test_full_handle __LOC__ (tcp, None)
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''')) ;
  let seg'''' = { seg with flags = Segment.Flags.singleton `FIN } in
  Alcotest.(check test_full_handle __LOC__ (tcp, None)
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg'''')) ;
  let seg''''' = { seg with flags = Segment.Flags.of_list [ `SYN ; `FIN ] } in
  Alcotest.(check test_full_handle __LOC__ (tcp, None)
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''''')) ;
  (* syn with data -- we ack the SYN only, sender expected to re-send data *)
  let seg'''''' = { seg with flags = Segment.Flags.singleton `SYN ; payload = Cstruct.create 100 } in
  Alcotest.(check test_handle __LOC__ (tcp', Some (your_ip, ans))
              (Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad seg''''''))

(*
let syn_rcvd () =
  let syn = { basic_seg with flags = Segment.Flags.singleton `SYN ; seq = Sequence.of_int32 23l } in
  let tcp', _ = Input.handle_segment tcp (Mtime.of_uint64_ns 0L) quad syn in
  (* now, the syn lead to a new connection, and a syn-ack being transmitted *)
  (* anything apart from an ACK segment should be dropped with reset *)
*)

let tests = [
  "no state", `Quick, no_state ;
  "listen", `Quick, listen ;
]
