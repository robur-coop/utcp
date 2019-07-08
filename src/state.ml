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
  snd_wnd : int ; (* send window (32 bit should be enough for everyone!?) *)
  snd_wl1 : Sequence.t ; (* sequence number used for last window update *)
  snd_wl2 : Sequence.t ; (* ack number used for last window update *)
  iss : Sequence.t ; (* initial send sequence number *)
  rcv_nxt : Sequence.t ; (* receive next *)
  rcv_wnd : int ; (* receive window *)
  irs : Sequence.t (* inital receive sequence number *)
}

let pp_control ppf c =
  Fmt.pf ppf "send unacknowledget %a send next %a send window %d \
              last window update sequence %a last window update ack %a \
              send initial sequence number %a \
              receive window %d receive next %a \
              receive initial sequence number %a"
    Sequence.pp c.snd_una Sequence.pp c.snd_nxt c.snd_wnd
    Sequence.pp c.snd_wl1 Sequence.pp c.snd_wl2
    Sequence.pp c.iss
    c.rcv_wnd Sequence.pp c.rcv_nxt Sequence.pp c.irs

let compare_int (a : int) (b : int) = compare a b

module Connection = struct
  type t = Ipaddr.V4.t * int * Ipaddr.V4.t * int

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
  read_queue : Cstruct.t list ;
  write_queue : Cstruct.t list ;
}

module IS = Set.Make(struct type t = int let compare = compare_int end)

type t = {
  listeners : IS.t ;
  connections : conn_state CM.t
}

let start_listen t port = { t with listeners = IS.add port t.listeners }
let stop_listen t port = { t with listeners = IS.remove port t.listeners }

let empty = { listeners = IS.empty ; connections = CM.empty }
