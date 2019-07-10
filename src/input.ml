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

(* from netsem:
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
    (* TODO options: mss, timestamp, window scaling *)
    (* TODO compute buffer sizes: bandwidth-delay-product, rcvbufsize, sndbufsize, maxseg, snd_cwnd *)
    (* TODO start retransmission timer *)
    let reply =
      Segment.make_syn_ack control_block
        ~src_port:seg.dst_port ~dst_port:seg.src_port
    in
    ({ t with connections = CM.add id conn_state t.connections }, Some reply)
  with
  | Ok (t, reply) -> t, reply
  | Error () ->
    (* deliver_in_1b - we do less checks and potentially send more resets *)
    (* deliver_in_5 / deliver_in_6 *)
    t, Segment.dropwithreset seg

(* we likely need both escape hatches: drop segment and drop connection and (maybe) send reset *)
let handle_conn t id conn seg =
  Log.debug (fun m -> m "handle_conn %a" pp_conn_state conn);
  let add conn' = { t with connections = CM.add id conn' t.connections }
  and _drop () = { t with connections = CM.remove id t.connections }
  in
  let cb = conn.control_block in
  match conn.tcp_state with
  | Syn_sent ->
    (* deliver_in_2 / deliver_in_2a / deliver_in_2b *)
    assert false
  | Syn_received ->
    (* deliver_in_3c and syn_received parts of deliver_in_3 *)
    begin match
        (* some errors should lead to reset+drop (those who have the sequence and ack right), others to silent drop... *)
        (* TODO hostLTS:15801: [[SYN]] flag set may be set in the final segment of a simultaneous open :*)
        guard (Segment.Flags.only `ACK seg.Segment.flags) "only ACK flag" >>= fun () ->
        (* hostLTS:15828 :*)
        guard (Sequence.greater seg.Segment.ack cb.snd_una) "ack > snd_una" >>= fun () ->
        (* not (ack <= tcp_sock.cb.snd_una \/ ack > tcp_sock.cb.snd_max) *)
        guard (Sequence.greater_equal seg.Segment.seq cb.irs) "seq >= irs" >>= fun () ->
        (* we sent a fin (already) and our fin is acked stuff *)
        (* rtt measurement likely *)
        (* paws (di3_topstuff) *)
        (* expect (assume for now): no data in that segment !? *)
        guard (Sequence.greater_equal seg.Segment.ack cb.iss) "ack >= iss" >>| fun () ->
        (* update cb as well, una and window.. *)
        (* if not cantsendmore established else if ourfinisacked fin_wait2 else fin_wait_1 *)
        add { conn with tcp_state = Established }, None
      with
      | Ok (t, a) -> (t, a)
      | Error msg ->
        Log.err (fun m -> m "error in syn_received failed condition %s" msg);
        (t, None)
    end
  | _state -> (* always the same ;) *)
    (* window handling etc., we'll end up in established likely (unless fin has been sent or received) *)
    (* we diverge a bit from deliver_in_3 by first matching on state *)
    (* we should handle the flags fin and rst, syn explicitly, and validate sequence numbers! *)
    (* and need the cantrcvmore / cantsndmore flags *)
    (* and retransmission timers and queues *)
    (t, None)

(* as noted above, 3b is not relevant for us *)
let handle t ~src ~dst data =
  match Segment.decode_and_validate ~src ~dst data with
  | Error (`Msg msg) ->
    Log.err (fun m -> m "dropping invalid segment %s" msg);
    (t, [])
  | Ok (seg, id) ->
    (* deliver_in_3a deliver_in_4 are done now! *)
    Log.app (fun m -> m "%a -> %a valid TCP %a"
                Ipaddr.V4.pp src Ipaddr.V4.pp dst
                Segment.pp seg) ;
    let t', out = match CM.find_opt id t.connections with
      | None -> handle_noconn t id seg
      | Some conn -> handle_conn t id conn seg
    in
    (t', match out with
      | None ->
        Log.app (fun m -> m "no answer"); []
      | Some d ->
        Log.app (fun m -> m "answering with %a" Segment.pp d);
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
