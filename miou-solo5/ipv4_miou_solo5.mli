module Ethernet = Ethernet_miou_solo5
module ARPv4 = Arp_miou_solo5

type t

type packet =
  { src : Ipaddr.V4.t
  ; dst : Ipaddr.V4.t
  ; protocol : int
  ; uid : int }

and payload =
  | Slice of Slice_bstr.t
  | String of string

val create :
     ?to_expire:int
  -> Ethernet.t
  -> ARPv4.t
  -> ?gateway:Ipaddr.V4.t
  -> ?handler:((packet * payload) -> unit)
  -> Ipaddr.V4.Prefix.t
  -> (t, [> `MTU_too_small ]) result

val max : t -> int
val src : t -> Ipaddr.V4.t

module Writer : sig
  type ipv4 = t
  type t

  val of_string : ipv4 -> string -> t
  val of_strings : ipv4 -> string list -> t
  val into : ipv4 -> len:int -> (Bstr.t -> unit) -> t

  type ('p, 'q, 'a) m
  type z
  type 'a s

  val ( let* ) : ('p, 'q, 'a) m -> ('a -> ('q, 'r, 'b) m) -> ('p, 'r, 'b) m
  val ( let+ ) : ('p s, 'q s, 'a) m -> (Bstr.t -> int) -> ('p, 'q s, 'a) m
  val return : 'a -> ('p, 'p, 'a) m
  val unknown : (z, 'n s, unit) m -> t
end

val write :
     t
  -> ?ttl:int
  -> ?src:Ipaddr.V4.t
  -> Ipaddr.V4.t
  -> protocol:int
  -> Writer.t
  -> (unit, [> `Route_not_found ]) result
(** [write ?ttl ?src dst protocol ?finally ?size sstr] writes a new IPv4
    packet (fragmented or not) to the specified destination [dst].

    The layer above IPv4 (notably TCP) may require the IPv4 "pseudo-header"
    before generating its own header (notably to calculate a checksum). The
    [finally] function is called with this pseudo-header and must return the
    header of the layer above IPv4. The size of this header must be known in
    advance via the [size] argument. *)

val input : t -> Slice_bstr.t Ethernet.packet -> unit
val set_handler : t -> ((packet * payload) -> unit) -> unit
