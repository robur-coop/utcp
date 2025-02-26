
module Make (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.t) : sig
  include Tcpip.Tcp.S with type ipaddr = Ip.ipaddr

  val connect : string -> Ip.t -> t
end
