
module Make (R : Mirage_crypto_rng_mirage.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.t) : sig
  include Tcpip.Tcp.S with type ipaddr = Ip.ipaddr

  val connect : string -> Ip.t -> t
end
