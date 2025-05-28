module Ethernet = Ethernet_miou_solo5
module ARPv4 = Arp_miou_solo5
module IPv4 = Ipv4_miou_solo5
module ICMPv4 = Icmpv4_miou_solo5
module UDPv4 = Udpv4_miou_solo5

exception Net_unreach
exception Closed_by_peer
exception Connection_refused

module TCPv4 : sig
  type state
  type flow
  type daemon

  val handler : state -> IPv4.packet * IPv4.payload -> unit
  val create : name:string -> IPv4.t -> daemon * state
  val kill : daemon -> unit

  val connect : state -> (Ipaddr.V4.t * int) -> flow
  val read : flow -> ?off:int -> ?len:int -> bytes -> int
  val really_read : flow -> ?off:int -> ?len:int -> bytes -> unit
  val write : flow -> ?off:int -> ?len:int -> string -> unit
  val close : flow -> unit
  val peers : flow -> (Ipaddr.t * int) * (Ipaddr.t * int)

  type listen

  val listen : state -> int -> listen
  val accept : state -> listen -> flow
  val udpv4 : state -> UDPv4.state
end

type tcpv4

val tcpv4 :
     name:string
  -> ?gateway:Ipaddr.V4.t
  -> Ipaddr.V4.Prefix.t
  -> (tcpv4  * TCPv4.state) Miou_solo5.arg

val kill : tcpv4 -> unit
