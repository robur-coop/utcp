type 'a state = 'a State.t

let empty = State.empty

let start_listen = State.start_listen

let stop_listen = State.stop_listen

type flow = State.Connection.t

let pp_flow = State.Connection.pp

let num_connections = State.num_connections

let peers (src, src_port, dst, dst_port) =
  (src, src_port), (dst, dst_port)

type output = Ipaddr.t * Ipaddr.t * Segment.t

let timer = Tcptimer.timer

let handle_buf = Input.handle_buf

let connect = User.connect

type tcp_state = State.tcp_state

let tcp_state_to_string = State.fsm_to_string

type error = [ `Not_found | `Bad_state of string * tcp_state | `Msg of string ]

let pp_error ppf = function
  | `Not_found -> Fmt.string ppf "not found"
  | `Bad_state (exp, st) ->
    Fmt.pf ppf "bad state: expected %s, but connection is in %s"
      exp (tcp_state_to_string st)
  | `Msg msg -> Fmt.string ppf msg

let close = User.close

let shutdown = User.shutdown

let recv = User.recv

let send = User.send

let force_enqueue = User.force_enqueue

module Segment = Segment

module Sequence = Sequence

module Timers = Timers

module State = State

module Input = Input

module User = User

module Checksum = Checksum

module Rope = Rope
