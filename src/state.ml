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

(* TODO:
  - missing ticks for timers
  - last_ack for delayed ack (what about delayed ack? is it worth it, even though SACK?)
  - duplicate acks count
  - congestion window, slow-start threshold
*)
type control_block = {
  snd_una : Sequence.t ; (* send unacknowledged *)
  snd_nxt : Sequence.t ; (* send next *)
  (* snd_wnd : int ; (\* send window (32 bit should be enough for everyone!?) *\) *)
  snd_wl1 : Sequence.t ; (* sequence number used for last window update *)
  snd_wl2 : Sequence.t ; (* ack number used for last window update *)
  iss : Sequence.t ; (* initial send sequence number *)
  rcv_nxt : Sequence.t ; (* receive next *)
  rcv_wnd : int ; (* receive window *)
  irs : Sequence.t (* inital receive sequence number *)
}

let pp_control ppf c =
  Fmt.pf ppf "snd_una %a snd_nxt %a \
              snd_wl1 %a snd_wl2 %a iss %a@. \
              rcv_wnd %d rcv_nxt %a irs %a"
    Sequence.pp c.snd_una Sequence.pp c.snd_nxt
    Sequence.pp c.snd_wl1 Sequence.pp c.snd_wl2
    Sequence.pp c.iss
    c.rcv_wnd Sequence.pp c.rcv_nxt Sequence.pp c.irs

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
  control_block : control_block ;
  (* reassembly : Cstruct.t list ; (* TODO nicer data structure! *) *)
  (* read_queue : Cstruct.t list ;
   * write_queue : Cstruct.t list ; *)
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

let start_listen t port = { t with listeners = IS.add port t.listeners }
let stop_listen t port = { t with listeners = IS.remove port t.listeners }

let empty rng ip = { rng ; ip ; listeners = IS.empty ; connections = CM.empty }
