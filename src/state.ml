(* (c) 2017-2019 Hannes Mehnert, all rights reserved *)

let src = Logs.Src.create "tcp.tracing" ~doc:"TCP tracing"
module Tracing = (val Logs.src_log src : Logs.LOG)

(* in contrast to literature, there is no need for LISTEN nor CLOSED --
   there's no tcp socket for them anyways *)
type tcp_state =
  | Syn_sent
  | Syn_received
  | Established
  | Close_wait
  | Fin_wait_1
  | Closing
  | Last_ack
  | Fin_wait_2
  | Time_wait

let behind_established = function Syn_sent | Syn_received -> false | _ -> true

let is_connected = function
  | Established | Close_wait | Fin_wait_1 | Closing | Last_ack | Fin_wait_2 -> true
  | _ -> false

let fsm_to_string = function
  | Syn_received -> "syn received"
  | Syn_sent -> "syn sent"
  | Established -> "established"
  | Fin_wait_1 -> "fin wait 1"
  | Fin_wait_2 -> "fin wait 2"
  | Closing -> "closing"
  | Time_wait -> "time wait"
  | Close_wait -> "close wait"
  | Last_ack -> "last ack"

let pp_fsm ppf s =
  Fmt.string ppf (fsm_to_string s)

(* hostTypes:182 *)
type rttinf = {
  t_rttupdated : int ; (*: number of times rtt sampled :*)
  tf_srtt_valid : bool ; (*: estimate is currently believed to be valid :*)
  t_srtt : Duration.t ; (*: smoothed round-trip time :*)
  t_rttvar : Duration.t ; (*: variance in round-trip time :*)
  t_rttmin : Duration.t ; (*: minimum rtt allowed :*)
  t_lastrtt : Duration.t option ; (*: most recent instantaneous RTT obtained :*)
  (*: Note this should really be an option type which is set to [[NONE]] if no
    value has been obtained. The same applies to [[t_lastshift]] below. :*)
  (* in BSD, this is the local variable rtt in tcp_xmit_timer(); we put it here
     because we don't want to store rxtcur in the tcpcb *)
  t_lastshift : int option ; (*: the last retransmission shift used :*)
  t_wassyn : bool (*: whether that shift was [[RexmtSyn]] or not :*)
  (* these two also are to avoid storing rxtcur in the tcpcb; they are somewhat
     annoying because they are *only* required for the tcp_output test that
     returns to slow start if the connection has been idle for >=1RTO *)
}

let pp_rttinf ppf t =
  Fmt.pf ppf "rttinf: #updated %u@ valid %B@ smoothed %a@ variance %a@ min %a@ \
              last %a@ shift %a@ wassyn %B"
    t.t_rttupdated t.tf_srtt_valid Duration.pp t.t_srtt Duration.pp t.t_rttvar
    Duration.pp t.t_rttmin Fmt.(option ~none:(any "none") Duration.pp) t.t_lastrtt
    Fmt.(option ~none:(any "none") int) t.t_lastshift t.t_wassyn

type rexmtmode = RexmtSyn | Rexmt | Persist

let mode_of = function
  | None -> None
  | Some ((x, _), _) -> Some x

module Reassembly_queue = struct
  type reassembly_segment = {
    seq : Sequence.t ;
    fin : bool ;
    data : Cstruct.t list ; (* in reverse order *)
  }

  (* we take care that the list is sorted by the sequence number *)
  type t = reassembly_segment list

  let empty = []

  let is_empty = function [] -> true | _ -> false

  let length t = List.length t

  let pp_rseg ppf { seq ; data ; _ } =
    Fmt.pf ppf "%a (len %u)" Sequence.pp seq (Cstruct.lenv data)

  let pp = Fmt.(list ~sep:(any ", ") pp_rseg)

  (* insert segment, potentially coalescing existing ones *)
  let insert_seg t (seq, fin, data) =
    (* they may overlap, the newest seg wins *)
    (* (1) figure out the place whereafter to insert the seg *)
    (* (2) peek whether the next seg can be already coalesced *)
    let inserted, segq =
      List.fold_left (fun (inserted, acc) e ->
          match inserted with
          | Some (elt, seq_end) ->
            (* 2 - the current "e" may be merged into the head of acc *)
            let acc' = match acc with [] -> [] | _hd :: tl -> tl in
            if Sequence.less_equal e.seq seq_end then
              let overlap = Sequence.sub seq_end e.seq in
              if overlap = 0 then
                (* overlap = 0, we can just merge them *)
                let elt = { elt with fin = e.fin || elt.fin ; data = e.data @ elt.data } in
                Some (elt, Sequence.addi elt.seq (Cstruct.lenv elt.data)), elt :: acc'
              else
                (* we need to cut some bytes from e *)
                let data = List.rev (Cstruct.shiftv (List.rev e.data) overlap) in
                let data = data @ elt.data in
                let elt = { elt with fin = e.fin || elt.fin ; data } in
                Some (elt, Sequence.addi elt.seq (Cstruct.lenv data)), elt :: acc'
            else
              (* there's still a hole, nothing to merge *)
              (inserted, e :: acc)
          | None ->
            (* 1 *)
            (* there are three cases:
               - (a) the new seq is before the existing e.seq -> prepend
                     (and figure out whether to merge with e)
                     seq <= e.seq
               - (b) the new seq is within e.seq + len e -> append (partially)
                     seq <= e.seq + len
               - (c) the new seq is way behind e.seq + len e -> move along
                     seq > e.seq + len
            *)
            if Sequence.less_equal seq e.seq then
              (* case (a) *)
              let seq_e = Sequence.addi seq (Cstruct.length data) in
              (* case (1): a new segment that is way before the existing one:
                 seq <= e.seq && seq_e <= e.seq -> e must be retained
                 case (2): a new segment that is partially before the existing:
                 seq <= e.seq && seq_e > e.seq -> e may be partially retained:
                  (i) seq_e >= e.seq_e -> drop e
                  (ii) seq_e < e.seq_e -> retain the last bytes of e
              *)
              if Sequence.less_equal seq_e e.seq then
                if Sequence.equal seq_e e.seq then
                  let e = { seq ; fin = fin || e.fin ; data = e.data @ [ data ] } in
                  Some (e, Sequence.addi seq (Cstruct.lenv e.data)), e :: acc
                else
                  let e' = { seq ; fin ; data = [ data ] } in
                  Some (e', Sequence.addi seq (Cstruct.length data)), e :: e' :: acc
              else
                let e_seq_e = Sequence.addi e.seq (Cstruct.lenv e.data) in
                if Sequence.greater_equal seq_e e_seq_e then
                  let e' = { seq ; fin ; data = [ data ] } in
                  Some (e', seq_e), e' :: acc
                else
                  (* we've to retain some parts of seq *)
                  let post =
                    let retain_data = Sequence.sub e_seq_e seq_e in
                    let skip_data = Cstruct.lenv e.data - retain_data in
                    Cstruct.shiftv (List.rev e.data) skip_data
                  in
                  let e = { seq ; fin = fin || e.fin ; data = List.rev (data :: post) } in
                  Some (e, Sequence.addi seq (Cstruct.lenv e.data)), e :: acc
            else
              let e_seq_e = Sequence.addi e.seq (Cstruct.lenv e.data) in
              if Sequence.less_equal seq e_seq_e then
                (* case (b) we append to the thing *)
                if Sequence.equal seq e_seq_e then
                  let e = { e with fin = fin || e.fin ; data = data :: e.data } in
                  Some (e, Sequence.addi e_seq_e (Cstruct.length data)), e :: acc
                else
                  let overlap = Sequence.sub e_seq_e seq in
                  let pre =
                    let rec cut_some amount = function
                      | [] -> []
                      | hd :: tl ->
                        if Cstruct.length hd < amount then
                          cut_some (amount - Cstruct.length hd) tl
                        else
                          Cstruct.sub hd amount (Cstruct.length hd - amount) :: tl
                    in
                    cut_some overlap e.data
                  in
                  let seq_e = Sequence.addi seq (Cstruct.length data) in
                  let end_ = Sequence.max e_seq_e seq_e in
                  let post =
                    if Sequence.greater e_seq_e seq_e then
                      let retain_data = Sequence.sub e_seq_e seq_e in
                      let skip_data = Cstruct.lenv e.data - retain_data in
                      Cstruct.shiftv (List.rev e.data) skip_data
                    else
                      []
                  in
                  let e = { e with fin = fin || e.fin ; data = post @ data :: pre } in
                  Some (e, end_), e :: acc
              else
                (None, e :: acc))
        (None, []) t
    in
    let segq =
      if inserted = None then
        { seq ; fin ; data = [ data ] } :: segq
      else
        segq
    in
    List.rev segq

  let maybe_take t seq =
    let r, t' =
      List.fold_left (fun (r, acc) e ->
          match r with
          | None ->
            if Sequence.equal seq e.seq then
              Some (Cstruct.concat (List.rev e.data), e.fin), acc
            else if Sequence.greater seq e.seq then
              let e_end = Sequence.addi e.seq (Cstruct.lenv e.data) in
              if Sequence.less seq e_end then
                let to_cut = Sequence.sub seq e.seq in
                let data = Cstruct.concat (List.rev e.data) in
                Some (Cstruct.shift data to_cut, e.fin), acc
              else
                None, acc
            else
              None, e :: acc
          | Some _ -> (r, e :: acc))
        (None, []) t
    in
    List.rev t', r
end

(* hostTypes:230 but dropped urg and ts stuff *)
type control_block = {
  (*: timers :*)
  (* TODO pretty sure we can consolidate them to one or two fields *)
  (* additionally, not all are allowed in all tcp states *)
  tt_rexmt : (rexmtmode * int) Timers.timed option; (*: retransmit timer, with mode and shift; [[NONE]] is idle :*)
    (*: see |tcp_output.c:356ff| for more info. :*)
    (*: as in BSD, the shift starts at zero, and is incremented each
        time the timer fires.  So it is zero during the first interval,
        1 after the first retransmit, etc. :*)
  (* tt_keep : unit Timers.timed option ; (\*: keepalive timer :*\) *)
  tt_2msl : unit Timers.timed option ; (*: $2*\mathit{MSL}$ [[TIME_WAIT]] timer :*)
  tt_delack : unit Timers.timed option ; (*: delayed [[ACK]] timer :*)
  tt_conn_est : unit Timers.timed option ; (*: connection-establishment timer, overlays keep in BSD :*)
  tt_fin_wait_2 : unit Timers.timed option ; (*: [[FIN_WAIT_2]] timer, overlays 2msl in BSD :*)
  t_idletime : Mtime.t ; (*: time since last segment received :*)

  (*: flags, some corresponding to BSD |TF_| flags :*)
  tf_needfin : bool ;
  tf_shouldacknow : bool ;

  (*: send variables :*)
  snd_una : Sequence.t ; (*: lowest unacknowledged sequence number :*)
  snd_max : Sequence.t ; (*: highest sequence number sent; used to recognise retransmits :*)
  snd_nxt : Sequence.t ; (*: next sequence number to send :*)
  snd_wl1 : Sequence.t ; (*: seq number of most recent window update segment :*)
  snd_wl2 : Sequence.t ; (*: ack number of most recent window update segment :*)
  iss : Sequence.t ; (* initial send sequence number *)
  snd_wnd : int ; (*: send window size: always between 0 and 65535*2**14 :*)
  snd_cwnd : int ; (*: congestion window :*)
  snd_ssthresh : int ; (*: threshold between exponential and linear [[snd_cwnd]] expansion (for slow start):*)

  (*: receive variables :*)
  rcv_wnd : int ; (*: receive window size :*)
  tf_rxwin0sent : bool ; (*: have advertised a zero window to receiver :*)
  rcv_nxt : Sequence.t ; (*: lowest sequence number not yet received :*)
  irs : Sequence.t ; (*: initial receive sequence number :*)
  rcv_adv : Sequence.t ; (*: most recently advertised window :*)
  last_ack_sent : Sequence.t ; (*: last acknowledged sequence number :*)

  (*: connection parameters :*)
  (* TODO move into tcp_state, at least t_advmss; tf_doing_ws/request_r_scale *)
  (* we also don't need that many options: we will do window scaling and MSS! *)
  t_maxseg : int ; (*: maximum segment size on this connection :*)
  t_advmss : int ; (*: the mss advertisment sent in our initial SYN :*)

  (* currently: false, None, 0, 0 in initial_cb;
     deliver_in_1 sets tf_doing_ws, request_r_scale, snd_scale, rcv_scale
     connect_1 sets request_r_scale
     Segment.make_syn/make_syn_ack use request_r_scale!
     deliver_in_2 sets tf_doing_ws, snd_scale, rcv_scale
     timer_tt_rexmtsyn may set request_r_scale to None
     --> only once we're in established, the values should be used! (retransmissions handle this?)
 *)
  tf_doing_ws : bool ; (*: doing window scaling on this connection?  (result of negotiation) :*)
  request_r_scale : int option ; (*: pending window scaling, if any (used during negotiation) :*)
  snd_scale : int ; (*: window scaling for send window (0..14), applied to received advertisements (RFC1323) :*)
  rcv_scale : int ; (*: window scaling for receive window (0..14), applied when we send advertisements (RFC1323) :*)

  (*: round-trip time estimation :*)
  t_rttseg : (Mtime.t * Sequence.t) option ; (*: start time and sequence number of segment being timed :*)
  t_rttinf : rttinf ; (*: round-trip time estimator values :*)

  (*: retransmission :*)
  t_dupacks : int ; (*: number of consecutive duplicate acks received (typically 0..3ish; should this wrap at 64K/4G ack burst?) :*)
  t_badrxtwin : Mtime.t ; (*: deadline for bad-retransmit recovery :*)
  snd_cwnd_prev : int ; (*: [[snd_cwnd]] prior to retransmit (used in bad-retransmit recovery) :*)
  snd_ssthresh_prev : int ; (*: [[snd_ssthresh]] prior to retransmit (used in bad-retransmit recovery) :*)
  snd_recover : Sequence.t ; (*: highest sequence number sent at time of receipt of partial ack (used in RFC2581/RFC2582 fast recovery) :*)

  (*: other :*)
  t_segq :  Reassembly_queue.t;  (*: segment reassembly queue :*)
  t_softerror : string option      (*: current transient error; reported only if failure becomes permanent :*)
  (*: could cut this down to the actually-possible errors? :*)

}

(* auxFns:1066*)
let initial_cb =
  let initial_rttinf = {
    t_rttupdated = 0;
    tf_srtt_valid = false;
    t_srtt = Params.tcptv_rtobase;
    t_rttvar = Params.tcptv_rttvarbase;
    t_rttmin = Params.tcptv_min;
    t_lastrtt = None;
    t_lastshift = None;
    t_wassyn = false  (* if t_lastshift=0, this doesn't make a difference *)
  } in
  {
    (* <| t_segq            := []; *)
    tt_rexmt = None;
    (* tt_keep = None; *)
    tt_2msl = None;
    tt_delack = None;
    tt_conn_est = None;
    tt_fin_wait_2 = None;
    tf_needfin = false;
    tf_shouldacknow = false;
    snd_una = Sequence.zero;
    snd_max = Sequence.zero;
    snd_nxt = Sequence.zero;
    snd_wl1 = Sequence.zero;
    snd_wl2 = Sequence.zero;
    iss = Sequence.zero;
    snd_wnd = 0;
    snd_cwnd = Params.tcp_maxwin lsl Params.tcp_maxwinscale;
    snd_ssthresh = Params.tcp_maxwin lsl Params.tcp_maxwinscale;
    rcv_wnd = 0;
    tf_rxwin0sent = false;
    rcv_nxt = Sequence.zero;
    irs = Sequence.zero;
    rcv_adv = Sequence.zero;
    snd_recover = Sequence.zero;
    t_maxseg = Params.mssdflt;
    t_advmss = Params.mssdflt;
    t_rttseg = None;
    t_rttinf = initial_rttinf ;
    t_dupacks = 0;
    t_idletime = Mtime.of_uint64_ns 0L;
    t_segq = Reassembly_queue.empty ;
    t_softerror = None;
    snd_scale = 0;
    rcv_scale = 0;
    request_r_scale = None;
    tf_doing_ws = false;
    last_ack_sent = Sequence.zero;
    snd_cwnd_prev = 0;
    snd_ssthresh_prev = 0;
    t_badrxtwin = Mtime.of_uint64_ns 0L;
  }

let pp_timer now ppf (_, deadline) =
  let now_span = Mtime.Span.of_uint64_ns (Mtime.to_uint64_ns now) in
  Duration.pp ppf
    (Mtime.to_uint64_ns
       (Option.value ~default:Mtime.min_stamp (Mtime.sub_span deadline now_span)))

let pp_rexmt now ppf ((mode, shift), deadline) =
  Fmt.pf ppf "%s, shift %u, deadline %a"
    (match mode with RexmtSyn -> "syn" | Rexmt -> "rexmt" | Persist -> "persist")
    shift (pp_timer now) ((), deadline)

let pp_control now ppf c =
  Fmt.pf ppf "needfin %B@ shouldacknow %B@ snd_una %a@ snd_max %a@ snd_nxt %a@ \
              snd_wl1 %a@ snd_wl2 %a@ iss %a@ snd_wnd %d@ snd_cwnd %d@ \
              snd_sshtresh %d@ rcv_wnd %d@ tf_rxwin0sent %B@ rcv_nxt %a@ \
              irs %a@ rcv_adv %a@ snd_recover %a@ t_maxseg %d@ t_advmss %d@ \
              snd_scale %d@ rcv_scale %d@ request_r_scale %a@ tf_doing_ws %B@ \
              tt_rexmt %a@ tt_2msl %a@ tt_delack %a@ tt_conn_est %a@ \
              tt_fin_wait_2 %a@ dupacks %u@ rttinf %a@ rttseg %a"
    c.tf_needfin c.tf_shouldacknow
    Sequence.pp c.snd_una Sequence.pp c.snd_max Sequence.pp c.snd_nxt
    Sequence.pp c.snd_wl1 Sequence.pp c.snd_wl2 Sequence.pp c.iss
    c.snd_wnd c.snd_cwnd c.snd_ssthresh c.rcv_wnd c.tf_rxwin0sent
    Sequence.pp c.rcv_nxt Sequence.pp c.irs Sequence.pp c.rcv_adv
    Sequence.pp c.snd_recover c.t_maxseg c.t_advmss
    c.snd_scale c.rcv_scale Fmt.(option ~none:(any "no") int) c.request_r_scale c.tf_doing_ws
    Fmt.(option ~none:(any "none") (pp_rexmt now)) c.tt_rexmt
    Fmt.(option ~none:(any "none") (pp_timer now)) c.tt_2msl
    Fmt.(option ~none:(any "none") (pp_timer now)) c.tt_delack
    Fmt.(option ~none:(any "none") (pp_timer now)) c.tt_conn_est
    Fmt.(option ~none:(any "none") (pp_timer now)) c.tt_fin_wait_2
    c.t_dupacks pp_rttinf c.t_rttinf
    Fmt.(option ~none:(any "none") (pair ~sep:(any ", ")
                                      (any "-" ++ Duration.pp) Sequence.pp))
    (Option.map (fun (ts, seg) ->
         let sent = Mtime.Span.of_uint64_ns (Mtime.to_uint64_ns ts) in
         let ts' =
           Mtime.to_uint64_ns
             (Option.value ~default:Mtime.min_stamp (Mtime.sub_span now sent))
         in
         ts', seg) c.t_rttseg)
(*
    (* tt_keep = None; *)
    t_idletime = Mtime.of_uint64_ns 0L;
    t_softerror = None;
    snd_cwnd_prev = 0;
    snd_ssthresh_prev = 0;
    t_badrxtwin = Mtime.of_uint64_ns 0L;
    last_ack_sent = Sequence.zero;
  *)

let compare_int (a : int) (b : int) = compare a b

module Connection = struct
  type t =
    { to_compare : string
    ; key : Ipaddr.t * int * Ipaddr.t * int }

  let inj_ipaddr value ~off buf =
    match value with
    | Ipaddr.V4 v4 ->
        Bytes.set_uint8 buf off 1;
        Ipaddr.V4.write_octets_exn ~off:(off + 1) v4 buf
    | Ipaddr.V6 v6 ->
        Bytes.set_uint8 buf off 2;
        Ipaddr.V6.write_octets_exn ~off:(off + 1) v6 buf

  let prj_ipaddr str ~off =
    match str.[0] with
    | '\001' -> Ipaddr.V4 (Ipaddr.V4.of_octets_exn ~off:(off + 1) str)
    | '\002' -> Ipaddr.V6 (Ipaddr.V6.of_octets_exn ~off:(off + 1) str)
    | _ -> assert false

  let inj (src, srcp, dst, dstp) =
    let len = (16 * 2) + 2 + (2 * 2) in
    let buf = Bytes.make len '\000' in
    inj_ipaddr src ~off:0 buf;
    Bytes.set_uint16_be buf 17 srcp;
    inj_ipaddr dst ~off:19 buf;
    Bytes.set_uint16_be buf 36 dstp;
    Bytes.unsafe_to_string buf

  let v key = { to_compare= inj key; key }
  let prj t = t.key

  let pp ppf t =
    let (src, srcp, dst, dstp) = t.key in
    Fmt.pf ppf "%a:%d -> %a:%d" Ipaddr.pp src srcp Ipaddr.pp dst dstp

  let pp_debug ppf (src, srcp, dst, dstp) =
    Fmt.pf ppf "%a:%d -> %a:%d" Ipaddr.pp src srcp Ipaddr.pp dst dstp

  let compare a b = String.compare a.to_compare b.to_compare
end

(* in this we store Connection.t -> state *)
module CM = Map.Make(Connection)

(* maybe timer information should go in here?
   -- put into tcp_state (allowing SYN_SENT (and closing states) to be slimmer)?
   -- segments to be retransmitted need to be preserved as well somewhere!
   --> and they may change whenever an ACK is received *)
(* sndq/rcvq: ownership discipline - as defined by the docs:
  - listen (mirage-net): the ownership of packet is transferred to the callback
  - send (mirage-flow) says that buffer ownership is now at the flow
*)
type 'a conn_state = {
  tcp_state : tcp_state ;
  control_block : control_block ; (* control_block should go into state, allowing smaller control blocks for initial states *)
  cantrcvmore : bool ;
  cantsndmore : bool ;
  rcvbufsize : int ;
  sndbufsize : int ;
  rcvq : Cstruct.t list ; (* reverse of the received data *)
  sndq : Cstruct.t list ; (* reverse list of data to be sent out *)
  rcv_notify : 'a;
  snd_notify : 'a;
  created : Mtime.t;
}

let conn_state created mk_notify ~rcvbufsize ~sndbufsize tcp_state control_block = {
  tcp_state ; control_block ;
  cantrcvmore = false ; cantsndmore = false ;
  rcvq = [] ; sndq = [] ;
  rcvbufsize ; sndbufsize ;
  rcv_notify = mk_notify () ; snd_notify = mk_notify () ;
  created ;
}

let pp_conn_state now ppf c =
  let created_span = Mtime.Span.of_uint64_ns (Mtime.to_uint64_ns c.created) in
  Fmt.pf ppf "TCP (since %a) %a cb %a"
    Duration.pp
    (Mtime.to_uint64_ns
       (Option.value ~default:Mtime.min_stamp (Mtime.sub_span now created_span)))
    pp_fsm c.tcp_state (pp_control now) c.control_block

module IS = Set.Make(struct type t = int let compare = compare_int end)

module Stats = struct
  type t = {
    mutable total_established : int ;
    mutable total_passive_connections : int ;
    mutable total_active_connections : int ;
  }

  let empty () = {
    total_established = 0 ;
    total_passive_connections = 0 ;
    total_active_connections = 0 ;
  }

  let incr_passive t =
    t.total_passive_connections <- succ t.total_passive_connections

  let incr_established t =
    t.total_established <- succ t.total_established

  let incr_active t =
    t.total_active_connections <- succ t.total_active_connections
end

(* path mtu (its global to a stack) *)
type 'a t = {
  rng : int -> string ;
  listeners : IS.t ;
  connections : 'a conn_state CM.t ;
  stats : Stats.t ;
  id : string ;
  mutable ctr : int ;
  metrics : (string -> Metrics.field list, Mtime.t * 'a conn_state CM.t * Stats.t -> Metrics.data) Metrics.src;
  transitions : (string -> Metrics.field list, string -> Metrics.data) Metrics.src;
  mk_notify : unit -> 'a;
}

module States = Map.Make (struct
    type t = tcp_state
    let compare a b = compare a b
  end)

let src = Logs.Src.create "tcp.state" ~doc:"TCP state"
module Log = (val Logs.src_log src : Logs.LOG)

let metrics () =
  let tcp_states =
    [ Syn_sent ; Syn_received ; Established ; Close_wait ; Fin_wait_1 ;
      Closing ; Last_ack ; Fin_wait_2 ; Time_wait
    ]
  in
  let open Metrics in
  let doc = "uTCP metrics" in
  let data (now, connections, stats) =
    let rcvq, sndq, states =
      CM.fold (fun k conn (rcvq, sndq, acc) ->
          if Mtime.(Span.to_uint64_ns (span now conn.created)) > Duration.of_min 1 then
            Log.info (fun m -> m "%a in %a" Connection.pp k (pp_conn_state now) conn);
          rcvq + Cstruct.lenv conn.rcvq,
          sndq + Cstruct.lenv conn.sndq,
          States.update conn.tcp_state (fun v -> Some (succ (Option.value ~default:0 v))) acc)
        connections
        (0, 0, States.empty)
    in
    let total = States.fold (fun _ v acc -> v + acc) states 0 in
    Data.v
      (List.map (fun tcp_state ->
           let v = Option.value ~default:0 (States.find_opt tcp_state states) in
           int (fsm_to_string tcp_state) v)
          tcp_states @ [
         int "active connections" total
       ; int "total established" stats.Stats.total_established
       ; int "total server" stats.total_passive_connections
       ; int "total client" stats.total_active_connections
       ; int "receive queue size" rcvq
       ; int "send queue size" sndq
       ])
  in
  let tag = Tags.string "stack-id" in
  Src.v ~doc ~tags:Tags.[ tag ] ~data "utcp"

let add_metrics t now =
  Metrics.add t.metrics (fun x -> x t.id) (fun d -> d (now, t.connections, t.stats))

let transitions () =
  let create () =
    let data : (string, int) Hashtbl.t = Hashtbl.create 7 in
    (fun key ->
       let cur = match Hashtbl.find_opt data key with
         | None -> 0
         | Some x -> x
       in
       Hashtbl.replace data key (succ cur)),
    (fun () ->
       let data, total =
         Hashtbl.fold (fun key value (acc, total) ->
             (Metrics.uint key value :: acc), value + total)
           data ([], 0)
       in
       Metrics.uint "total" total :: data)
  in
  let open Metrics in
  let doc = "uTCP transition metrics" in
  let incr, get = create () in
  let data thing = incr thing; Data.v (get ()) in
  let tag = Tags.string "stack-id" in
  Src.v ~doc ~tags:Metrics.Tags.[ tag ] ~data "utcp_transition"

let rule t name =
  Metrics.add t.transitions (fun x -> x t.id) (fun d -> d name)

let pp now ppf t =
  Fmt.pf ppf "listener %a, connections: %a"
    Fmt.(list ~sep:(any ", ") int) (IS.elements t.listeners)
    Fmt.(list ~sep:(any "@.") (pair ~sep:(any ": ") Connection.pp (pp_conn_state now)))
    (CM.bindings t.connections)

let start_listen t port = { t with listeners = IS.add port t.listeners }
let stop_listen t port = { t with listeners = IS.remove port t.listeners }

let empty mk_notify id rng =
  {
    id ;
    rng ;
    listeners = IS.empty ;
    connections = CM.empty ;
    stats = Stats.empty () ;
    ctr = 0 ;
    metrics = metrics () ;
    transitions = transitions () ;
    mk_notify ;
  }
