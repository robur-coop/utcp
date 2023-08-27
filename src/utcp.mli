type state

val empty : (int -> Cstruct.t) -> state

val start_listen : state -> int -> state

val stop_listen : state -> int -> state

type flow

module FM : Map.S with type key = flow

val pp_flow : flow Fmt.t

val peers : flow -> (Ipaddr.t * int) * (Ipaddr.t * int)

module Sequence : sig
  type t

  val of_int32 : int32 -> t
  val to_int32 : t -> int32

  val zero : t

  val add : t -> t -> t
  val incr : t -> t

  val addi : t -> int -> t

  val window : t -> t -> int

  val less : t -> t -> bool
  val less_equal : t -> t -> bool
  val greater : t -> t -> bool
  val greater_equal : t -> t -> bool
  val equal : t -> t -> bool

  val min : t -> t -> t
  val max : t -> t -> t

  val pp : t Fmt.t
end

module Segment : sig
  type tcp_option =
    | MaximumSegmentSize of int
    | WindowScale of int
    | Unknown of int * Cstruct.t

  type t = {
    src_port : int ;
    dst_port : int ;
    seq : Sequence.t ;
    ack : Sequence.t option ;
    flag : [ `Syn | `Fin | `Rst ] option ;
    push : bool ;
    window : int ;
    options : tcp_option list ;
    payload : Cstruct.t ;
  }

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val decode_and_validate : src:Ipaddr.t -> dst:Ipaddr.t -> Cstruct.t ->
    (t * flow, [ `Msg of string ]) result

  val encode_and_checksum : src:Ipaddr.t -> dst:Ipaddr.t -> t -> Cstruct.t
end

type output = Ipaddr.t * Ipaddr.t * Segment.t

val timer : state -> Mtime.t -> (state * flow list * output list)

val handle_buf : state -> Mtime.t -> src:Ipaddr.t -> dst:Ipaddr.t ->
  Cstruct.t ->
  (state * [ `Established of flow | `Drop of flow | `Received of flow ] option * output option)

val connect : src:Ipaddr.t -> ?src_port:int -> dst:Ipaddr.t -> dst_port:int ->
  state -> Mtime.t -> (state * flow * output)

val close : state -> Mtime.t -> flow ->
  (state * output option, [ `Msg of string ]) result

val shutdown : state -> Mtime.t -> flow -> [ `read | `write | `read_write ] ->
  (state * output option, [ `Msg of string ]) result

val recv : state -> flow -> (state * Cstruct.t, [ `Msg of string | `Eof ]) result

val send : state -> Mtime.t -> flow -> Cstruct.t ->
  (state * output option, [ `Msg of string ]) result

(**/**)
(* only to be used for testing! *)
module Timers : sig
  type 'a timed = 'a * Mtime.t
end

module State : sig
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
  type rttinf = private {
    t_rttupdated : int ;
    tf_srtt_valid : bool ;
    t_srtt : Duration.t ;
    t_rttvar : Duration.t ;
    t_rttmin : Duration.t ;
    t_lastrtt : Duration.t option ;
    t_lastshift : int option ;
    t_wassyn : bool
  }
  type rexmtmode = private RexmtSyn | Rexmt | Persist
  type control_block = {
    tt_rexmt : (rexmtmode * int) Timers.timed option;
    tt_2msl : unit Timers.timed option ;
    tt_delack : unit Timers.timed option ;
    tt_conn_est : unit Timers.timed option ;
    tt_fin_wait_2 : unit Timers.timed option ;
    t_idletime : Mtime.t ;
    tf_needfin : bool ;
    tf_shouldacknow : bool ;
    snd_una : Sequence.t ;
    snd_max : Sequence.t ;
    snd_nxt : Sequence.t ;
    snd_wl1 : Sequence.t ;
    snd_wl2 : Sequence.t ;
    iss : Sequence.t ;
    snd_wnd : int ;
    snd_cwnd : int ;
    snd_ssthresh : int ;
    rcv_wnd : int ;
    tf_rxwin0sent : bool ;
    rcv_nxt : Sequence.t ;
    irs : Sequence.t ;
    rcv_adv : Sequence.t ;
    last_ack_sent : Sequence.t ;
    t_maxseg : int ;
    t_advmss : int ;
    tf_doing_ws : bool ;
    request_r_scale : int option ;
    snd_scale : int ;
    rcv_scale : int ;
    t_rttseg : (Mtime.t * Sequence.t) option ;
    t_rttinf : rttinf ;
    t_dupacks : int ;
    t_badrxtwin : Mtime.t ;
    snd_cwnd_prev : int ;
    snd_ssthresh_prev : int ;
    snd_recover : Sequence.t ;
    t_softerror : string option
  }
  val initial_cb : control_block
  type conn_state = {
    tcp_state : tcp_state ;
    control_block : control_block ;
    cantrcvmore : bool ;
    cantsndmore : bool ;
    rcvbufsize : int ;
    sndbufsize : int ;
    sndq : Cstruct.t ;
    rcvq : Cstruct.t ;
  }
  val conn_state : rcvbufsize:int -> sndbufsize:int -> tcp_state ->
    control_block -> conn_state
  module IS : Set.S with type elt = int
  module CM : Map.S with type key = flow
  type t = {
    rng : int -> Cstruct.t ;
    listeners : IS.t ;
    connections : conn_state CM.t
  }
  val pp : t Fmt.t
  val empty : (int -> Cstruct.t) -> t
  val start_listen : t -> int -> t
  val stop_listen : t -> int -> t
end

module Input : sig
  val handle_segment : State.t -> Mtime.t -> flow -> Segment.t ->
    State.t * (Ipaddr.t * Ipaddr.t * Segment.t) option
end

module User : sig
  val connect : src:Ipaddr.t -> ?src_port:int -> dst:Ipaddr.t -> dst_port:int ->
    State.t -> Mtime.t -> (State.t * flow * output)
end
(**/**)
