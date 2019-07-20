(* (c) 2017-2019 Hannes Mehnert, all rights reserved *)

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

let equal_tcp_state a b = match a, b with
  | Syn_sent, Syn_sent
  | Syn_received, Syn_received
  | Established, Established
  | Close_wait, Close_wait
  | Fin_wait_1, Fin_wait_1
  | Closing, Closing
  | Last_ack, Last_ack
  | Fin_wait_2, Fin_wait_2
  | Time_wait, Time_wait -> true
  | _ -> false

let behind_established = function Syn_sent | Syn_received -> false | _ -> true

let is_connected = function
  | Established | Close_wait | Fin_wait_1 | Closing | Last_ack | Fin_wait_2 -> true
  | _ -> false

let pp_fsm ppf s =
  Fmt.string ppf @@
  match s with
  | Syn_received -> "syn received"
  | Syn_sent -> "syn sent"
  | Established -> "established"
  | Fin_wait_1 -> "fin wait 1"
  | Fin_wait_2 -> "fin wait 2"
  | Closing -> "closing"
  | Time_wait -> "time wait"
  | Close_wait -> "close wait"
  | Last_ack -> "last ack"

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

let equal_rttinf a b =
  a.t_rttupdated = b.t_rttupdated &&
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

type rexmtmode = RexmtSyn | Rexmt | Persist

let mode_of = function
  | None -> None
  | Some ((x, _), _) -> Some x

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
  (* t_segq :  tcpReassSegment list;  (\*: segment reassembly queue :*\) *)
  t_softerror : string option      (*: current transient error; reported only if failure becomes permanent :*)
  (*: could cut this down to the actually-possible errors? :*)

}

let equal_timer_opt eq a b = match a, b with
  | None, None -> true
  | Some (a, d), Some (b, d') -> d = d' && eq a b
  | _ -> false

let equal_control_block a b =
  equal_timer_opt
    (fun (mode, shift) (mode', shift') -> mode = mode' && shift = shift')
    a.tt_rexmt b.tt_rexmt &&
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

let pp_control ppf c =
  Fmt.pf ppf "needfin %B@ shouldacknow %B@ snd_una %a@ snd_max %a@ snd_nxt %a@ snd_wl1 %a@ snd_wl2 %a@ iss %a@ \
              snd_wnd %d@ snd_cwnd %d@ snd_sshtresh %d@ \
              rcv_wnd %d@ tf_rxwin0sent %B@ rcv_nxt %a@ irs %a@ src_adv %a@ \
              snd_recover %a@ t_maxseg %d@ t_advmss %d@ snd_scale %d@ rcv_scale %d@ request_r_scale %a@ tf_doing_ws %B"
    c.tf_needfin c.tf_shouldacknow
    Sequence.pp c.snd_una Sequence.pp c.snd_max Sequence.pp c.snd_nxt
    Sequence.pp c.snd_wl1 Sequence.pp c.snd_wl2 Sequence.pp c.iss
    c.snd_wnd c.snd_cwnd c.snd_ssthresh c.rcv_wnd c.tf_rxwin0sent
    Sequence.pp c.rcv_nxt Sequence.pp c.irs Sequence.pp c.rcv_adv
    Sequence.pp c.snd_recover c.t_maxseg c.t_advmss
    c.snd_scale c.rcv_scale Fmt.(option ~none:(unit "no") int) c.request_r_scale c.tf_doing_ws
(*
    tt_rexmt = None;
    (* tt_keep = None; *)
    tt_2msl = None;
    tt_delack = None;
    tt_conn_est = None;
    tt_fin_wait_2 = None;
    t_rttseg = None;
    t_rttinf = initial_rttinf ;
    t_dupacks = 0;
    t_idletime = Mtime.of_uint64_ns 0L;
    t_softerror = None;
    snd_cwnd_prev = 0;
    snd_ssthresh_prev = 0;
    t_badrxtwin = Mtime.of_uint64_ns 0L;
    last_ack_sent = Sequence.zero;
  *)

let compare_int (a : int) (b : int) = compare a b

module Connection = struct
  type t = Ipaddr.V4.t * int * Ipaddr.V4.t * int

  let pp ppf (src, srcp, dst, dstp) =
    Fmt.pf ppf "%a:%d -> %a:%d" Ipaddr.V4.pp src srcp Ipaddr.V4.pp dst dstp

  let andThen a b = if a = 0 then b else a
  let compare ((src, srcp, dst, dstp) : t) ((src', srcp', dst', dstp') : t) =
    andThen (compare_int srcp srcp')
      (andThen (compare_int dstp dstp')
         (andThen (Ipaddr.V4.compare src src')
            (Ipaddr.V4.compare dst dst')))
end

(* in this we store Connection.t -> state *)
module CM = Map.Make(Connection)

(* maybe timer information should go in here?
   -- put into tcp_state (allowing SYN_SENT (and closing states) to be slimmer)?
   -- segments to be retransmitted need to be preserved as well somewhere!
   --> and they may change whenever an ACK is received *)
type conn_state = {
  tcp_state : tcp_state ;
  control_block : control_block ; (* i think control_block should go into state *)
  cantrcvmore : bool ;
  cantsndmore : bool ;
  rcvbufsize : int ;
  sndbufsize : int ;
  sndq : Cstruct.t ;
  rcvq : Cstruct.t ;
  (* reassembly : Cstruct.t list ; (* TODO nicer data structure! *) *)
}

let equal_conn_state a b =
  equal_tcp_state a.tcp_state b.tcp_state &&
  a.cantrcvmore = b.cantrcvmore &&
  a.cantsndmore = b.cantsndmore &&
  a.rcvbufsize = b.rcvbufsize &&
  Cstruct.equal a.sndq b.sndq &&
  Cstruct.equal a.rcvq b.rcvq &&
  equal_control_block a.control_block b.control_block

let conn_state ~rcvbufsize ~sndbufsize tcp_state control_block = {
  tcp_state ; control_block ;
  cantrcvmore = false ; cantsndmore = false ;
  sndq = Cstruct.empty ; rcvq = Cstruct.empty ;
  rcvbufsize ; sndbufsize
}

let pp_conn_state ppf c =
  Fmt.pf ppf "TCP %a cb %a" pp_fsm c.tcp_state pp_control c.control_block

module IS = Set.Make(struct type t = int let compare = compare_int end)

(* path mtu (its global to a stack) *)
type t = {
  rng : int -> Cstruct.t ;
  ip : Ipaddr.V4.t ;
  listeners : IS.t ;
  connections : conn_state CM.t
}

let equal a b =
  Ipaddr.V4.compare a.ip b.ip = 0 &&
  IS.equal a.listeners b.listeners &&
  CM.equal equal_conn_state a.connections b.connections

let pp ppf t =
  Fmt.pf ppf "IP %a, listener %a, connections: %a"
    Ipaddr.V4.pp t.ip Fmt.(list ~sep:(unit ", ") int) (IS.elements t.listeners)
    Fmt.(list ~sep:(unit "@.") (pair ~sep:(unit ": ") Connection.pp pp_conn_state))
    (CM.bindings t.connections)

let quad t (a, ap, b, bp) =
  if Ipaddr.V4.compare a t.ip = 0 then a, ap, b, bp else b, bp, a, ap

let start_listen t port = { t with listeners = IS.add port t.listeners }
let stop_listen t port = { t with listeners = IS.remove port t.listeners }

let empty rng ip = { rng ; ip ; listeners = IS.empty ; connections = CM.empty }
