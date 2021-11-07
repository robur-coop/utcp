type state = State.t

let empty = State.empty

let start_listen = State.start_listen

let stop_listen = State.stop_listen

type flow = State.Connection.t

module FM = State.CM

let pp_flow = State.Connection.pp

let peers (src, src_port, dst, dst_port) =
  (src, src_port), (dst, dst_port)

type output = Ipaddr.t * Ipaddr.t * Cstruct.t

let timer = Tcptimer.timer

let handle_buf = Input.handle_buf

let connect = User.connect

let close = User.close

let recv = User.recv

let send = User.send

module Segment = Segment

module Sequence = Sequence

module Timers = Timers

module State = State

module Input = Input

module User = User
