(* (c) 2017 Hannes Mehnert, all rights reserved *)

let src = Logs.Src.create "tcp.input" ~doc:"TCP input"
module Log = (val Logs.src_log src : Logs.LOG)

open State

open Rresult.R.Infix

let guard p e = if p then Ok () else Error e

(* in general, some flag combinations are always bad:
    only one of syn, fin, rst can ever be reasonably set.
 *)

(* FreeBSD uses: inpcb, which points to socket (SO_ACCEPTCON) AND its pcb  *)
(*  pcb points back to its inpcb, socket has at least the functionality:
  sbdrop, abavail, sbcut_locked, .sb_hiwat, sbreserve_locked, .sb_flags, sbused,
  sbsndptr, sbsndmbuf, .sb_state (used for CANTRCVMORE), sbappendstream_locked, sbspace
 *)

(* input rules from netsem
deliver_in_1 - passive open - handle_unknown_fragment
deliver_in_1b - drop bad for listening - handle_unknown_fragment
deliver_in_2 - active open - handle_conn state conn
deliver_in_2a - bad or boring, RST or ignore - handle_conn state conn
deliver_in_2b - simultaneous open - handle_conn state conn
deliver_in_3 - data, fin, ack in established - handle_conn state conn
deliver_in_3a - data with invalid checksum - handle_conn state conn --> validate_segment fails
deliver_in_3b - data when process gone away - not handled
deliver_in_3c - stupid ack or land in SYN_RCVD - handle_conn state conn --> validate_segment fails
deliver_in_4 - drop non-sane or martian segment - handle_input ? difference from 3a?
deliver_in_5 - drop with RST sth not matching any socket - handle_unknown_fragment
deliver_in_6 - drop sane segment in CLOSED - handle_unknown_fragment (we may need CLOSED state for this -- receive and drop (silently!) a sane segment that matches a CLOSED socket)
deliver_in_7 - recv RST and zap - handle_con state conn (and maybe ignored (?LISTEN?. SYN_SENT, TIME_WAIT))
deliver_in_8 - recv SYN in yy - handle_conn state conn
deliver_in_9 - recv SYN in TIME_WAIT (in case there's no LISTEN) - not handled
*)

let handle_noconn t id seg =
  match
    (* TL;DR: if there's a listener, and it is a SYN, we do sth useful. otherwise RST *)
    guard (IS.mem seg.Segment.dst_port t.listeners) () >>= fun () ->
    guard (Segment.Flags.only `SYN seg.Segment.flags) () >>| fun () ->
    (* deliver_in_1 - passive open *)
    (* segment is acceptable -- checked above *)
    (* broadcast/multicast already handled by decode_and_validate *)
    (* best_match also done implicitly -- connection table already dealt with *)
    (* there can't be anything in TIME_WAIT, otherwise we wouldn't end up here *)
    (* TODO check RFC 1122 Section 4.2.2.13 whether this actually happens (socket reusage) *)
    (* TODO resource management: limit number of outstanding connection attempts *)
    let control_block =
      let iss = (* random value *) Sequence.of_int32 7l
      and ack = Sequence.incr seg.seq (* ACK the SYN *)
      and snd_wnd = 0 and rcv_wnd = 65000
      in
      { snd_una = iss ; snd_nxt = Sequence.incr iss ; snd_wnd ;
        snd_wl1 = Sequence.zero ; snd_wl2 = Sequence.zero ;
        iss ; rcv_nxt = ack ; rcv_wnd ; irs = seg.seq }
    in
    let conn_state =
      { tcp_state = Syn_received ; control_block ;
        read_queue = [] ; write_queue = [] }
    in
    (* TODO options: mss, window scaling *)
    (* TODO compute buffer sizes: bandwidth-delay-product, rcvbufsize, sndbufsize, maxseg, snd_cwnd *)
    (* TODO start retransmission timer *)
    let reply =
      Segment.make_syn_ack control_block
        ~src_port:seg.dst_port ~dst_port:seg.src_port
    in
    Log.debug (fun m -> m "%a passive open %a" Connection.pp id pp_conn_state conn_state);
    ({ t with connections = CM.add id conn_state t.connections }, Some reply)
  with
  | Ok (t, reply) -> t, reply
  | Error () ->
    (* deliver_in_1b - we do less checks and potentially send more resets *)
    (* deliver_in_5 / deliver_in_6 *)
    t, Segment.dropwithreset seg

let in_window cb seg =
  (* from table in 793bis13 3.3 *)
  let seq = seg.Segment.seq
  and max = Sequence.addi cb.rcv_nxt cb.rcv_wnd
  in
  match Cstruct.len seg.Segment.payload, cb.rcv_wnd with
  | 0, 0 -> Sequence.equal seq cb.rcv_nxt
  | 0, _ -> Sequence.less_equal cb.rcv_nxt seq && Sequence.less seq max
  | _, 0 -> false
  | dl, _ ->
    (*assert dl > 0*)
    let rseq = Sequence.addi seq (pred dl) in
    (Sequence.less_equal cb.rcv_nxt seq && Sequence.less seq max) ||
    (Sequence.less_equal cb.rcv_nxt rseq && Sequence.less rseq max)

(* we likely need both escape hatches: drop segment and drop connection and (maybe) send reset *)
let handle_conn t id conn seg =
  Log.debug (fun m -> m "handle_conn %a %a seg %a" Connection.pp id pp_conn_state conn Segment.pp seg);
  let add conn' =
    Log.debug (fun m -> m "%a now %a" Connection.pp id pp_conn_state conn');
    { t with connections = CM.add id conn' t.connections }
  and drop () =
    Log.debug (fun m -> m "%a dropped" Connection.pp id);
    { t with connections = CM.remove id t.connections }
  in
  let cb = conn.control_block in
  match conn.tcp_state with
  | Syn_sent ->
    (* deliver_in_2 / deliver_in_2a / deliver_in_2b *)
    assert false
  | Syn_received ->
    (* deliver_in_3c and syn_received parts of deliver_in_3 *)
    begin match
        (* TODO hostLTS:15801: [[SYN]] flag set may be set in the final segment of a simultaneous open :*)
        (* what is the current state? *)
        (* - we acked the initial syn, their seq should be rcv_nxt (or?) *)
        (* - furthermore, it should be >= irs -- that's redundant with above *)
        (* if their seq is good (but their ack isn't or it is no ack), reset *)
        guard (Sequence.equal seg.Segment.seq cb.rcv_nxt) (`Drop "seq = rcv_nxt") >>= fun () ->
        (* - we sent our syn, so we expect an appropriate ack for the syn! *)
        (* - we didn't send out more data, so that ack should be exact *)
        (* if their seq is not good, drop packet *)
        guard (Segment.Flags.only `ACK seg.Segment.flags) (`Reset "only ACK flag") >>= fun () ->
        (* hostLTS:15828 - well, more or less ;) *)
        guard (Sequence.equal seg.Segment.ack cb.snd_nxt) (`Reset "ack = snd_nxt") >>| fun () ->
        (* not (ack <= tcp_sock.cb.snd_una \/ ack > tcp_sock.cb.snd_max) *)
        (* TODO we sent a fin (already) and our fin is acked stuff *)
        (* TODO rtt measurement likely *)
        (* paws (di3_topstuff) *)
        (* expect (assume for now): no data in that segment !? *)
        (* guard (Sequence.greater_equal seg.Segment.ack cb.iss) "ack >= iss" >>| fun () -> *)
        (* update other parts as well? wl1/wl2? *)
        let control_block = { cb with snd_una = seg.Segment.ack ; snd_wnd = seg.Segment.window } in
        (* if not cantsendmore established else if ourfinisacked fin_wait2 else fin_wait_1 *)
        add { conn with tcp_state = Established ; control_block }, None
      with
      | Ok (t, a) -> t, a
      | Error (`Drop msg) ->
        Log.err (fun m -> m "dropping segment in syn_received failed condition %s" msg);
        t, None
      | Error (`Reset msg) ->
        Log.err (fun m -> m "reset in syn_received %s" msg);
        drop (), Segment.dropwithreset seg
    end
  | state -> (* always the same ;) *)
    (* window handling etc., we'll end up in established likely (unless fin has been sent or received) *)
    (* we diverge a bit from deliver_in_3 by first matching on state *)
    (* we should handle the flags fin and rst, syn explicitly, and validate sequence numbers! *)
    (* and need the cantrcvmore / cantsndmore flags *)
    (* and retransmission timers and queues *)
    match
      (* sequence number and ack processing (drop or rst): *)
      guard (in_window cb seg) (`Drop "in_window") >>| fun () ->
      (* flag evaluation: what should we do with this segment? *)
      (* ACK processing *)
      (* RST + SYN processing -> immediate ack or drop *)
      (* FIN processing *)
      (* data processing *)
      t, None
    with
    | Ok (t, a) -> t, a
    | Error (`Drop msg) ->
      Log.err (fun m -> m "dropping segment in %a failed condition %s" pp_fsm state msg);
      t, None
    | Error (`Reset msg) ->
      Log.err (fun m -> m "reset in %a %s" pp_fsm state msg);
      drop (), Segment.dropwithreset seg

(* as noted above, 3b is not relevant for us *)
let handle t ~src ~dst data =
  match Segment.decode_and_validate ~src ~dst data with
  | Error (`Msg msg) ->
    Log.err (fun m -> m "dropping invalid segment %s" msg);
    (t, [])
  | Ok (seg, id) ->
    (* deliver_in_3a deliver_in_4 are done now! *)
    Log.info (fun m -> m "%a TCP %a" Connection.pp id Segment.pp seg) ;
    let t', out = match CM.find_opt id t.connections with
      | None -> handle_noconn t id seg
      | Some conn -> handle_conn t id conn seg
    in
    (t', match out with
      | None ->
        Log.info (fun m -> m "no answer"); []
      | Some d ->
        Log.info (fun m -> m "answer %a" Segment.pp d);
        [ `Data (src, Segment.encode_and_checksum ~src:dst ~dst:src d) ])

(*
- timer : t -> t * Cstruct.t list * [ `Timeout of connection | `Error of connection ]

other operations:
- create_connection : t -> ?src:int -> Ipaddr.t -> int -> t * connection * Cstruct.t
- close_connection : t -> connection -> t * Cstruct.t option
- write : t -> connection -> Cstruct.t -> t * Cstruct.t option [may also fail if connection bad or half-closed]

on individual sockets:
- shutdown_read
- shutdown_write

should we only hand out connection when it's established? *)

(* there's the ability to connect a socket to itself (using e.g. external fragments) *)
