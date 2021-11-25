
module Make_v4 (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Mirage_protocols.IPV4) : sig
  include Mirage_protocols.TCP
    with type ipaddr = Ip.ipaddr

  val connect : Ip.t -> t
end

module Make_v6 (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Mirage_protocols.IPV6) : sig
  include Mirage_protocols.TCP
    with type ipaddr = Ip.ipaddr

  val connect : Ip.t -> t
end

module type IPV4V6 = Mirage_protocols.IP with type ipaddr = Ipaddr.t

module Make_v4v6 (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : IPV4V6) : sig
  include Mirage_protocols.TCP
    with type ipaddr = Ip.ipaddr

  val connect : Ip.t -> t
end
