
module Make (R : Mirage_random.S) (Mclock : Mirage_clock.MCLOCK) (Time : Mirage_time.S) (Ip : Mirage_protocols.IPV4) : sig
  include Mirage_protocols.TCP
    with type ipaddr = Ip.ipaddr
     and type ipinput = src:Ip.ipaddr -> dst:Ip.ipaddr -> Cstruct.t -> unit Lwt.t

  val connect : Ip.t -> t
end
