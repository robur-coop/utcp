module IPv4 = Ipv4_miou_solo5

type state

val create : IPv4.t -> state

val recvfrom : state ->
  ?src:Ipaddr.V4.t ->
  port:int ->
  ?off:int -> ?len:int -> bytes -> int * (Ipaddr.V4.t * int)

val sendto: state ->
  dst:Ipaddr.V4.t ->
  ?src_port:int ->
  port:int ->
  ?off:int ->
  ?len:int -> string -> (unit, [> `Route_not_found ]) result

val handler : state -> IPv4.packet * IPv4.payload -> unit
