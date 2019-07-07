(* (c) 2017 Hannes Mehnert, all rights reserved *)

let src = Logs.Src.create "tcp.input" ~doc:"TCP input"
module Log = (val Logs.src_log src : Logs.LOG)

open State

open Rresult.R.Infix
(* in general, some flag combinations are always bad:
    only one of syn, fin, rst can ever be reasonably set.
 *)

(* FreeBSD uses: inpcb, which points to socket (SO_ACCEPTCON) AND its pcb  *)
(*  pcb points back to its inpcb, socket has at least the functionality:
  sbdrop, abavail, sbcut_locked, .sb_hiwat, sbreserve_locked, .sb_flags, sbused,
  sbsndptr, sbsndmbuf, .sb_state (used for CANTRCVMORE), sbappendstream_locked, sbspace
 *)

(* TCP input handling (no ICMP errors atm) *)
(* for any given incoming frame:
    - compute the identifier (dest-ip dest-port src-ip src-port)
    - lookup whether we've a connection state for it
    - if not, handle unknown incoming fragment:
      - maybe a SYN and some listener is interested (passive-open, inserting a new state into map)
      - maybe RST out if nothing found (and not an incoming RST)
    - if we've state, validate sequence numbers and possible move forward
      - or error out (dropping bad frame (keep current state), rst to remote (bad input, new/no state), new state and potential output
    - we also need a way to signal the caller to notify waiters (application data received, ..)
*)

(* from netsem:
<<>> validate sequence number only needs to happen in synchronised states!
deliver_in_1 - passive open - handle_unknown_fragment
deliver_in_1b - drop bad for listening - handle_unknown_fragment
deliver_in_2 - active open or simultaneous open - handle_conn state conn
deliver_in_2a - bad or boring, RST or ignore - handle_conn state conn
devlier_in_3 - data, fin, ack in established - handle_conn state conn
deliver_in_3a - data with invalid checksum - handle_conn state conn --> validate_segment fails
deliver_in_3b - data when process gone away - handle_conn state conn --> ??? fails (we can't really have this) [process gone away]
deliver_in_3c - stupid ack or land in SYN_RCVD - handle_conn state conn --> validate_segment fails
deliver_in_4 - drop non-sane or martian segment - handle_input ? difference from 3a?
deliver_in_5 - drop with RST sth not matching any socket - handle_unknown_fragment
deliver_in_6 - drop sane segment in CLOSED - handle_unknown_fragment (we may need CLOSED state for this -- receive and drop (silently!) a sane segment that matches a CLOSED socket)
deliver_in_7 - recv RST and zap - handle_con state conn (and maybe ignored (?LISTEN?. SYN_SENT, TIME_WAIT))
deliver_in_8 - recv SYN in yy - handle_conn state conn
deliver_in_9 - recv SYN in TIME_WAIT (in case there's no LISTEN) - handle_conn state conn
*)

let handle t ~src ~dst data =
  Segment.decode_and_validate ~src ~dst data >>= fun seg ->
  Log.app (fun m -> m "%a -> %a valid TCP %a"
              Ipaddr.V4.pp src Ipaddr.V4.pp dst
              Segment.pp seg) ;
  Ok (t, [])
(*  let id = Connection.of_segment src dst segment in
  (* deliver_in_4: validate checksum (and non-martian) *)
  match CM.find_opt id t.connections with
  | None -> (* may be 1 1b 5 6 *)

  | Some _ -> assert false (* may be 2 2a 3 3a 3b 3c 7 8 9 *)
*)
(*
so, what's the protocol?
handle_input : t -> Cstruct.t ->
t * Cstruct.t option * [ `Data of connection * Cstruct.t | `Connected of int * connection | `Error of connection ]

and t needs to contain:
 - listener sockets
 - active connections (a collection thereof)
   - control block
   - buffers

operations for server sockets:
- listen : t -> int -> t
- close : t -> int -> t

- create_connection : t -> ?src:int -> Ipaddr.t -> int -> t * connection * Cstruct.t
- close_connection : t -> connection -> t * Cstruct.t option
- write : t -> connection -> Cstruct.t -> t * Cstruct.t option [may also fail if connection bad or half-closed]

- timer : t -> t * Cstruct.t list * [ `Timeout of connection | `Error of connection ]

should we only hand out connection when it's established?
*)

(*
let handle_input pcb seg =
  (* TODO: may need tick as well *)
  (* TODO: needs an rng as well *)
  (* we would usually _first_ check that seg.seq is good, _but_ there are
     some states where sequence is not yet initialised *)
  (* since we don't really ever want to fail, the return type hereof is
     (pcb list * seg option) <- a set of pcbs to replace pcb with, and an optional out seg *)
  match pcb.fsm with
  | Listen ->
    (* deliver_in_1 the standard passive open! *)
    (* according to 1025 FIN is fine as well (lamp-test packet), we ignore it*)
    (* we also drop data of syn silently (we could reject syn with data) *)
    if Segment.has_flags ~yes:[ `SYN ] ~no:[ `ACK ; `RST ] seg then begin
      (* TODO: incomplete connection queue (backlog)? *)
      (* TODO: syncache / syncookies? *)
      let iss = Sequence.of_int32 42l in
      (* TODO: option handling:
         - MSS (40..65535)
         - timestamp if syn contained timestamp and we want timestamp
         - window scaling (same, if they want and we want)
         - sack *)
      (* TODO: figure out rcvbufsize, sendbufsize, maxseg, cwnd -
           look at bandwidth delay product, and incoming params, and host resources *)
      let pcb' = { empty_control with
                   fsm = Syn_received ;
                   send_unack = iss ;
                   send_next = Sequence.succ iss ;
                   send_max = Sequence.succ iss ;
                   send_initial = iss ;
                   send_window = seg.Segment.window_size ;
                   receive_window = ??? ;
                   receive_next = Sequence.succ seg.Segment.sequence ;
                   receive_advertised = ??? ;
                   receive_initial = seg.Segment.sequence ;
                   send_congestion_window = ??? ;
                   send_slowstart_threshold = ???
                 }
      in
      let out = { sequence = iss ; ack_seq = Sequence.succ seg.Segment.sequence ;
                  flags = Segment.or_flags [ `SYN ; `ACK ] ; window_size = ??? }
      in
      [pcb::pcb'], Some out
    end else
      pcb, Segment.maybe_rst seg
  | Syn_sent ->
    (* deliver_in_2:
       - completion of active open [syn+ack with right ack] (-> established. out: ack)
       - simultaneous open [syn no ack] (-> syn_rcvd, out: ack)
       - if RST is set (-> closed ++ delete pcb)
       --> but if in the meantime our socket was close()d (cantsndmore) -> FIN_WAIT_X [2 if our FIN is acked (how did this happen?), 1 if not] *)
    (* there may be data in this segment! *)
    if Segment.has_flags ~yes:[ `SYN ; `ACK ] ~no:[ `RST ] seg then begin
      (* ack was for our syn! *)
      if Sequence.less pcb.send_initial seg.Segment.ack && Sequence.less_equal seg.Segment.ack pcb.send_max then
        let r_scale, s_scale = match seg.Segment.options.r_scale, pcb.s_scale with ... in
        let ts = match seg.Segment.options.ts, pcb.ts with ... in
        (* cc_conn_init *)
        let mss = match .. with in
        let rcvbufsize, sendbufsize, maxseg, send_congestion_window = ... in
        let receive_window = calculate_window sock in
        (* RTT measurement *)
        (* timers *)
        let data = .. in
        let rcvq = data in
        let receive_next = seq + 1 (* for SYN *) + length data in
        let receive_window = receive_window - length data in
        (* update cb => established *)
        (* send ack *)
      else
        maybe_rst seg
    end else if Segment.has_flags ~yes:[ `SYN ] ~no:[ `ACK ; `RST ] seg then begin
      (* simultaneous open => syn_rcvd *)
    end else if Segment.has_flags ~yes:[ `RST ] ~no:[ `SYN ; `ACK ] seg then begin
      (* receive reset, connection dropped => closed | drop *)
    end else (* if ACK or ACK&RST *)
      maybe_rst
  | Syn_received ->
*)

(* in general, there are three sources of events we need to consider:
  - a segment was received for this flow
  - user action (connect/close/shutdown) <- close(w)|shutdown(r,w) leads to (w=)cantsndmore/(r=)cantrcvmore -> if set, output adds FIN
          cantrcvmore is set by an incoming FIN, and then read returns EOF
  - user intends to send (maybe better during a timer look into out queue)
  - a timer occured
*)

(* there's the ability to connect a socket to itself (using e.g. external fragments) *)
