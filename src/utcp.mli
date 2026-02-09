(** µTCP - an implementation of the Transmission Control Protocol

    TCP is a widely used protocol on the Internet. µTCP has its origins in the
    {{:https://www.cl.cam.ac.uk/~pes20/Netsem/}Netsem (network semantics)
    research project}, which is an executable specification of TCP/IP and the
    Unix sockets API in HOL4.

    µTCP was manually translated from the HOL4 specification into OCaml, and
    decisions were taken where needed. There is no support for out of band data
    (urgent pointers). TCP timestamps are also not supported (due to its dubious
    use).

    TCP is mainly specified in {{:https://tools.ietf.org/html/rfc9293}RFC 9293}.
*)

(** {1 Abstract state type} *)

(** The abstract type of an immutable state of the TCP stack. *)
type 'a state

(** [empty make_notify id] constructs an empty TCP state with a function
    how to create a notifications, and an identifier for the TCP stack. *)
val empty : (unit -> 'a) -> string -> 'a state

(** [start_listen t port] adds [port] to the set of listened to ports. *)
val start_listen : 'a state -> int -> 'a state

(** [stop_listen t port] removes [port] from the set of listened to ports. *)
val stop_listen : 'a state -> int -> 'a state

(** The abstract type of a flow - a connection between this TCP stack and a remote endpoint. *)
type flow

(** [pp_flow flow] pretty-prints the [flow]. *)
val pp_flow : flow Fmt.t

(** [peers flow] is the two endpoints of the [flow], each identifiable by their
    IP and port. *)
val peers : flow -> (Ipaddr.t * int) * (Ipaddr.t * int)

(** The module of TCP sequence numbers (unsigned 32 bit values, arithmetic operations may wrap) *)
module Sequence : sig

  (** The abstract type of a sequence number. *)
  type t

  (** [of_int32 i] converts [i] to a sequence number. *)
  val of_int32 : int32 -> t

  (** [to_int32 t] converts [t] to an int32. *)
  val to_int32 : t -> int32

  (** [zero] is the 0 as sequence number. *)
  val zero : t

  (** [add s1 s2] adds sequence number [s1] to [s2]. *)
  val add : t -> t -> t

  (** [incr s] increments the sequence number [s] by 1. *)
  val incr : t -> t

  (** [addi s i] adds [i] to the sequence number [s]. *)
  val addi : t -> int -> t

  (** [window s1 s2] computes the window (amount of bytes) between [s1] and [s2]. *)
  val window : t -> t -> int

  (** [less s1 s2] checks whether [s1] is smaller than [s2]. *)
  val less : t -> t -> bool

  (** [less_equal s1 s2] checks whether [s1] is smaller than [s2] or equal to [s2]. *)
  val less_equal : t -> t -> bool

  (** [greater s1 s2] checks whether [s1] is greater than [s2]. *)
  val greater : t -> t -> bool

  (** [greater_equal s1 s2] checks whether [s1] is greater than [s2] or equal to [s2]. *)
  val greater_equal : t -> t -> bool

  (** [equal s1 s2] checks whether [s1] is equal to [s2]. *)
  val equal : t -> t -> bool

  (** [min s1 s2] returns the minimum of [s1] and [s2]. *)
  val min : t -> t -> t

  (** [max s1 s2] returns the maximum of [s1] and [s2]. *)
  val max : t -> t -> t

  (** [pp s] pretty-prints the sequence number [s]. *)
  val pp : t Fmt.t
end

(** The module of TCP segments, as seen on the wire. *)
module Segment : sig

  (** The variant type of supported TCP options *)
  type tcp_option =
    | MaximumSegmentSize of int
    | WindowScale of int
    | Unknown of int * Cstruct.t

  (** The record type of the TCP segment *)
  type t = {
    src_port : int ;
    dst_port : int ;
    seq : Sequence.t ;
    ack : Sequence.t option ;
    flag : [ `Syn | `Fin | `Rst ] option ;
    push : bool ;
    window : int ;
    options : tcp_option list ;
    payload : string list ;
    payload_len : int ;
  }

  (** [pp seg] pretty-prints the segment [seg]. *)
  val pp : t Fmt.t

  (** [equal s1 s2] is true if [s1] and [s2] are equal (comparing all fields of the record). *)
  val equal : t -> t -> bool

  (** [decode_and_validate ~src ~dst data] decodes [data] and validates its
      checksum. Some further checks are done: broadcast and multicast packets
      error, also packets to and from the same endpoint (equal IP address and
      equal port). If the result is Ok, it is a segment and the flow.
  *)
  val decode_and_validate : src:Ipaddr.t -> dst:Ipaddr.t -> Cstruct.t ->
    (t * flow, [ `Msg of string ]) result

  (** [length seg] computes the length in bytes of [s]. *)
  val length : t -> int

  (** [encode_and_checksum now ~src ~dst seg] encodes the segment [seg] into a
      buffer, and computes its checksum. The value [now] is only used for
      logging.  *)
  val encode_and_checksum : Mtime.t -> src:Ipaddr.t -> dst:Ipaddr.t -> t -> Cstruct.t

  (** [encode_and_checksum_into now buf ~src ~dst seg] encodes the segment [seg]
      into the buffer [buf], and computes its checksum. The value [now] is only
      used for logging. *)
  val encode_and_checksum_into : Mtime.t -> Cstruct.t -> src:Ipaddr.t -> dst:Ipaddr.t -> t -> unit

  (** [checksum ~src ~dst buf] computes the checksum of [buf]. *)
  val checksum : src:Ipaddr.t -> dst:Ipaddr.t -> Cstruct.t -> int

  (** [encode seg] encodes the segment [seg] into a buffer. *)
  val encode : t -> Cstruct.t
end

(** The type for outputting packets: a triple of source IP address, destination
    IP address, and segment. *)
type output = Ipaddr.t * Ipaddr.t * Segment.t

(** [timer state now] runs the TCP timer at [now] for [state]. It results in a
    new [state], a list of errors (a quadruple of flow, concrete error, and
    receive and send notification), and a list of segments to send out. *)
val timer : 'a state -> Mtime.t ->
  ('a state * (flow * [ `Retransmission_exceeded | `Timer_2msl | `Timer_connection_established | `Timer_fin_wait_2 ] * 'a * 'a) list * output list)

(** [handle_buf state now ~src ~dst buf] handles the buffer [buf] for the TCP
    stack. This results in a fresh state, optionally a change in the flow
    (Established, Drop, Signal), and a list of segments to send. *)
val handle_buf : 'a state -> Mtime.t -> src:Ipaddr.t -> dst:Ipaddr.t ->
  Cstruct.t ->
  ('a state * [ `Established of flow * 'a option
              | `Drop of flow * 'a option * 'a list
              | `Signal of flow * 'a list ] option * output list)

(** [connect ~src ?src_port ~dst ~dst_port state now] starts a TCP connection
    from [src, src_port] to [dst, dst_port]. The [src_port] will be picked at
    random if not provided. The output is a fresh TCP state, the flow, a
    notification, and a segment to send out. *)
val connect : src:Ipaddr.t -> ?src_port:int -> dst:Ipaddr.t -> dst_port:int ->
  'a state -> Mtime.t -> ('a state * flow * 'a * output)

(** [close state now flow] closes [flow]. It results either in a fresh TCP state
    and a list of segments to send out, or an error (if the [flow] cannot be
    found, or some other error). *)
val close : 'a state -> Mtime.t -> flow ->
  ('a state * output list, [ `Not_found | `Msg of string ]) result

(** [shutdown state now flow direction] shuts the [flow] down in the given
    [direction]. It results in a frsh TCP state and a list of segments to send
    out, or an error. *)
val shutdown : 'a state -> Mtime.t -> flow -> [ `read | `write | `read_write ] ->
  ('a state * output list, [ `Not_found | `Msg of string ]) result

(** [recv state now flow] receives data for [flow]. The read notification is
    also provided - if there's no awaiting data, this notification can be waited
    on. *)
val recv : 'a state -> Mtime.t -> flow ->
  ('a state * string list * 'a * output list, [ `Not_found | `Msg of string | `Eof ]) result

(** [send state now flow ~off ~len data] sends [data] on [flow], starting at
    [off] (defaults to 0) of length [len] (defaults to [data] until the end).
    This outputs a fresh TCP state, the number of bytes enqueued, the write
    notification, and a list of segments to send. *)
val send : 'a state -> Mtime.t -> flow -> ?off:int -> ?len:int -> string ->
  ('a state * int * 'a * output list, [ `Not_found | `Msg of string ]) result

(** [force_enqueue state now flow ~off ~len data] pushes [data] on [flow],
    starting at [off] (defaults to 0) of length [len] (defaults to [data] until
    the end) onto the send queue. This may exceed the send queue size, use with
    caution. *)
val force_enqueue : 'a state -> Mtime.t -> flow -> ?off:int -> ?len:int -> string ->
  ('a state, [ `Not_found | `Msg of string ]) result

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
  module Reassembly_queue : sig
    type t
    val empty : t
    val is_empty : t -> bool
    val length : t -> int
    val insert_seg : t -> (Sequence.t * bool * Rope.t) -> t
    val maybe_take : t -> Sequence.t -> (t * (Rope.t * bool) option)
    val pp : t Fmt.t
  end
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
    t_segq : Reassembly_queue.t ;
    t_softerror : string option
  }
  val initial_cb : control_block
  type 'a conn_state = {
    tcp_state : tcp_state ;
    control_block : control_block ;
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
  val conn_state : Mtime.t -> (unit -> 'a) -> rcvbufsize:int -> sndbufsize:int -> tcp_state ->
    control_block -> 'a conn_state
  module IS : Set.S with type elt = int
  module CM : Map.S with type key = flow
  module Stats : sig type t end
  type 'a t = {
    listeners : IS.t ;
    connections : 'a conn_state CM.t ;
    stats : Stats.t ;
    id : string ;
    mutable ctr : int ;
    metrics : (string -> Metrics.field list, Mtime.t * 'a conn_state CM.t * Stats.t -> Metrics.data) Metrics.src;
    transitions : (string -> Metrics.field list, string -> Metrics.data) Metrics.src;
    mk_notify : unit -> 'a;
  }
  val pp : Mtime.t -> 'a t Fmt.t
  val empty : (unit -> 'a) -> string -> 'a t
  val start_listen : 'a t -> int -> 'a t
  val stop_listen : 'a t -> int -> 'a t
end

module Input : sig
  val handle_segment : 'a State.t -> Mtime.t -> flow -> Segment.t ->
    'a State.t * (Ipaddr.t * Ipaddr.t * Segment.t) list
end

module User : sig
  val connect : src:Ipaddr.t -> ?src_port:int -> dst:Ipaddr.t -> dst_port:int ->
    'a State.t -> Mtime.t -> ('a State.t * flow * 'a * output)
end

module Checksum = Checksum
module Rope = Rope
(**/**)
