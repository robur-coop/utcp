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
    data : Rope.t ;
  }

  let pp_rseg ppf { seq ; data ; _ } =
    Fmt.pf ppf "%a (len %u)@ " Sequence.pp seq (Rope.length data)

  module Tree = struct
    type color = Red | Black
    type t =
      | Leaf
      | Node of color * reassembly_segment * t * t

    let balance = function
      | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
      | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
      | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
      | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | a, b, c, d -> Node (a, b, c, d)

    let insert t x =
      let rec go = function
        | Leaf -> Node (Red, x, Leaf, Leaf)
        | Node (co, y, l, r) ->
          if Sequence.less x.seq y.seq then balance (co, y, go l, r)
          else if Sequence.greater x.seq y.seq then balance (co, y, l, go r)
          else Node (co, x, l, r)
      in
      match go t with
      | Node (_, y, l, r) -> Node (Black, y, l, r)
      | Leaf -> assert false

    let balance_del = function
      | Black, z, Node (Red, y, Node (Red, x, a, b), c), d ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | Black, x, a, Node (Red, z, Node (Red, y, b, c), d) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | a, b, c, d -> Node (a, b, c, d)

    (* "Bubble up" a double-black by recoloring *)
    let bubble_left = function
      | Node (Black, y, Node (Red, x, a, b), right) ->
        balance_del (Black, y, Node (Red, x, a, b), right)
      | Node (co, y, left, Node (Black, z, a, b)) ->
        balance_del (co, y, left, Node (Red, z, a, b))
      | Node (co, y, left, Node (Red, x, Node (Black, z, a, b), c)) ->
        Node (co, x, Node (Black, y, left, Node (Red, z, a, b)), c)
      | t -> t

    let bubble_right = function
      | Node (co, y, Node (Black, x, a, b), right) ->
        balance_del (co, y, Node (Red, x, a, b), right)
      | Node (Black, z, Node (Red, x, a, Node (Black, y, b, c)), right) ->
        Node (Black, x, a, Node (Black, z, Node (Red, y, b, c), right))
      | t -> t

    let rec min_elt = function
      | Leaf -> assert false
      | Node (_, v, Leaf,  _) -> v
      | Node (_, _, left, _) -> min_elt left

    let rec remove_min = function
      | Leaf -> assert false
      | Node (_, _, Leaf, r) -> r
      | Node (c, v, l, r) ->
        let l' = remove_min l in
        bubble_left (Node (c, v, l', r))

    let remove t x =
      let rec rem = function
        | Leaf -> Leaf
        | Node (c, v, l, r) ->
          if Sequence.less x.seq v.seq then
            let l' = rem l in
            bubble_left (Node (c, v, l', r))
          else if Sequence.greater x.seq v.seq then
            let r' = rem r in
            bubble_right (Node (c, v, l, r'))
          else
            (* Found: remove this node *)
            match l, r with
            | Leaf, _ -> r
            | _, Leaf -> l
            | _ ->
              let s = min_elt r in
              let r' = remove_min r in
              bubble_right (Node (c, s, l, r'))
      in
      match rem t with
      | Node (_, v, l, r) -> Node (Black, v, l, r)
      | Leaf -> Leaf

    let find t seq =
      let rec go smaller = function
        | Leaf ->
          (match smaller with Leaf -> None | Node (_, v, _, _) -> Some v)
        | Node (_, v, l, r) as n ->
          if Sequence.less seq v.seq then go smaller l
          else if Sequence.greater seq v.seq then go n r
          else Some v
      in
      go Leaf t
  end

  type t = Tree.t

  let pp ppf tree =
    let rec go = function
      | Tree.Leaf -> ()
      | Node (_, v, l, r) ->
        go l ; pp_rseg ppf v ; go r
    in
    go tree

  let rec length = function
    | Tree.Leaf -> 0
    | Node (_, _, l, r) -> 1 + length l + length r

  let empty = Tree.Leaf

  let is_empty = function Tree.Leaf -> true | _ -> false

  (* insert segment, potentially coalescing existing ones *)
  let insert_seg t (seq, fin, (data : Rope.t)) =
    (* they may overlap, the newest seg wins *)
    (* (1) figure out the place whereafter to insert the seg *)
    (* (2) peek whether the next seg can be already coalesced *)
    let insert_with_end t elt =
      let seq_end = Sequence.addi elt.seq (Rope.length elt.data) in
      match Tree.find t seq_end with
      | None -> Tree.insert t elt
      | Some e ->
        if Sequence.less_equal elt.seq e.seq && Sequence.greater_equal seq_end e.seq then
          let t = Tree.remove t e in
          let overlap = Sequence.sub seq_end e.seq in
          let elt =
            if overlap = 0 then
              (* overlap = 0, we can just merge them *)
              { elt with fin = e.fin || elt.fin ; data = Rope.concat elt.data e.data }
            else
              (* we need to cut some bytes from e *)
              let data = Rope.shift e.data overlap in
              let data = Rope.concat elt.data data in
              { elt with fin = e.fin || elt.fin ; data }
          in
          let t =
            (* we need to get rid of the segments that are completely overlapped by the new one *)
            let rec rm_one t =
              match Tree.find t seq_end with
              | None -> t
              | Some e ->
                let eseqe = Sequence.addi e.seq (Rope.length e.data) in
                if Sequence.greater_equal e.seq elt.seq && Sequence.less_equal eseqe seq_end then
                  rm_one (Tree.remove t e)
                else
                  t
            in
            rm_one t
          in
          Tree.insert t elt
        else
          Tree.insert t elt
    in
    match Tree.find t seq with
    | None -> insert_with_end t { seq ; fin ; data }
    | Some e ->
      (* either the new one is disjoint or we can append *)
      let e_seqe = Sequence.addi e.seq (Rope.length e.data) in
      let elt =
        if Sequence.equal e_seqe seq then
          let data = Rope.concat e.data data in
          { e with fin = e.fin || fin ; data }
        else if Sequence.greater e_seqe seq then
          let overlap = Sequence.sub e_seqe seq in
          let pre = Rope.chop e.data (Rope.length e.data - overlap) in
          let data = Rope.concat pre data in
          { e with fin = e.fin || fin ; data }
        else
          { seq ; fin ; data }
      in
      insert_with_end t elt

  let maybe_take t seq =
    match Tree.find t seq with
    | None -> t, None
    | Some e ->
      Tree.remove t e,
      if Sequence.equal seq e.seq then
        Some (e.data, e.fin)
      else if Sequence.greater seq e.seq then
        let e_end = Sequence.addi e.seq (Rope.length e.data) in
        if Sequence.less seq e_end then
          let to_cut = Sequence.sub seq e.seq in
          let data = Rope.shift e.data to_cut in
          Some (data, e.fin)
        else
          None
      else
        None
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
  type t = Ipaddr.t * int * Ipaddr.t * int

  let pp ppf (src, srcp, dst, dstp) =
    Fmt.pf ppf "%a:%d -> %a:%d" Ipaddr.pp src srcp Ipaddr.pp dst dstp

  let andThen a b = if a = 0 then b else a
  let compare ((src, srcp, dst, dstp) : t) ((src', srcp', dst', dstp') : t) =
    andThen (compare_int srcp srcp')
      (andThen (compare_int dstp dstp')
         (andThen (Ipaddr.compare src src')
            (Ipaddr.compare dst dst')))
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
  rcvq : Rope.t ;
  sndq : Rope.t ;
  rcv_notify : 'a;
  snd_notify : 'a;
  created : Mtime.t;
}

let conn_state created mk_notify ~rcvbufsize ~sndbufsize tcp_state control_block = {
  tcp_state ; control_block ;
  cantrcvmore = false ; cantsndmore = false ;
  rcvq = Rope.empty ; sndq = Rope.empty ;
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
  listeners : IS.t ;
  connections : 'a conn_state CM.t ;
  stats : Stats.t ;
  id : string ;
  mutable ctr : int ;
  mk_notify : unit -> 'a;
}

module States = Map.Make (struct
    type t = tcp_state
    let compare a b = compare a b
  end)

let src = Logs.Src.create "tcp.state" ~doc:"TCP state"
module Log = (val Logs.src_log src : Logs.LOG)

let collect_metrics now connections =
  CM.fold (fun k conn (rcvq, sndq, acc) ->
      if Mtime.(Span.to_uint64_ns (span now conn.created)) > Duration.of_min 1 then
        Log.debug (fun m -> m "%a in %a" Connection.pp k (pp_conn_state now) conn);
      rcvq + Rope.length conn.rcvq,
      sndq + Rope.length conn.sndq,
      States.update conn.tcp_state (fun v -> Some (succ (Option.value ~default:0 v))) acc)
    connections
    (0, 0, States.empty)

let num_connections t = CM.cardinal t.connections

let metrics =
  let tcp_states =
    [ Syn_sent ; Syn_received ; Established ; Close_wait ; Fin_wait_1 ;
      Closing ; Last_ack ; Fin_wait_2 ; Time_wait
    ]
  in
  let open Metrics in
  let doc = "uTCP metrics" in
  let data ((rcvq, sndq, states), stats) =
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
  Metrics.add metrics (fun x -> x t.id) (fun d -> d (collect_metrics now t.connections, t.stats))

let transitions =
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
  Metrics.add transitions (fun x -> x t.id) (fun d -> d name)

let pp now ppf t =
  Fmt.pf ppf "listener %a, connections: %a"
    Fmt.(list ~sep:(any ", ") int) (IS.elements t.listeners)
    Fmt.(list ~sep:(any "@.") (pair ~sep:(any ": ") Connection.pp (pp_conn_state now)))
    (CM.bindings t.connections)

let start_listen t port = { t with listeners = IS.add port t.listeners }
let stop_listen t port = { t with listeners = IS.remove port t.listeners }

let empty mk_notify id =
  {
    id ;
    listeners = IS.empty ;
    connections = CM.empty ;
    stats = Stats.empty () ;
    ctr = 0 ;
    mk_notify ;
  }
