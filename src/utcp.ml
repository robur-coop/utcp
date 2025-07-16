type 'a state = 'a State.t

let empty = State.empty

let start_listen = State.start_listen

let stop_listen = State.stop_listen

type flow = State.Connection.t

let pp_flow = State.Connection.pp
let unsafe_flow = State.Connection.v

let peers conn =
  let (src, src_port, dst, dst_port) = State.Connection.prj conn in
  (src, src_port), (dst, dst_port)

type output = Ipaddr.t * Ipaddr.t * Segment.t

let timer = Tcptimer.timer

let handle_buf = Input.handle_buf

let connect = User.connect

let close = User.close

let shutdown = User.shutdown

let recv = User.recv

let send = User.send

module Segment = Segment

module Sequence = Sequence

module Timers = Timers

module State = State

module Input = Input

module User = User

module Checksum = Checksum
