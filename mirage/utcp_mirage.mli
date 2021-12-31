
module Make_v4 (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t) : sig
  include Tcpip.Tcp.S
    with type ipaddr = Ip.ipaddr

  val connect : Ip.t -> t
end

module Make_v6 (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.V6.t) : sig
  include Tcpip.Tcp.S
    with type ipaddr = Ip.ipaddr

  val connect : Ip.t -> t
end

module Make_v4v6 (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.t) : sig
  include Tcpip.Tcp.S
    with type ipaddr = Ip.ipaddr

  val connect : Ip.t -> t
end
