[@@@warning "-30"]

module Packet = struct
  type protocol =
    | ARPv4
    | IPv4
    | IPv6

  type t =
    { src : Macaddr.t
    ; dst : Macaddr.t
    ; protocol : protocol option }

  let guard err fn = if fn () then Ok () else Error err

  let protocol_of_int = function
    | 0x0806 -> Some ARPv4
    | 0x0800 -> Some IPv4
    | 0x86dd -> Some IPv6
    | _ -> None

  let protocol_to_int = function
    | ARPv4 -> 0x0806
    | IPv4 -> 0x0800
    | IPv6 -> 0x86dd

  let decode bstr ~len =
    let ( let* ) = Result.bind in
    let* () = guard `Invalid_ethernet_packet @@ fun () ->
      len >= 14 in
    let dst = Macaddr.of_octets_exn (Bstr.sub_string bstr ~off:0 ~len:6) in
    let src = Macaddr.of_octets_exn (Bstr.sub_string bstr ~off:6 ~len:6) in
    let protocol = Bstr.get_uint16_be bstr 12 in
    let protocol = protocol_of_int protocol in
    let payload = Slice_bstr.make ~off:14 ~len:(len - 14) bstr in
    Ok ({ src; dst; protocol }, payload)

  let encode_into t ?(off= 0) bstr = match t.protocol with
    | None -> Fmt.invalid_arg "Ethernet.Packet.encode_into: you must specify a protocol"
    | Some protocol ->
        let protocol = protocol_to_int protocol in
        Bstr.blit_from_string (Macaddr.to_octets t.dst) ~src_off:0 bstr ~dst_off:(off + 0) ~len:6;
        Bstr.blit_from_string (Macaddr.to_octets t.src) ~src_off:0 bstr ~dst_off:(off + 6) ~len:6;
        Bstr.set_uint16_be bstr 12 protocol
end

type protocol = Packet.protocol =
  | ARPv4
  | IPv4
  | IPv6

type t =
  { net : Miou_solo5.Net.t
  ; mutable handler : handler
  ; mtu : int
  ; mac : Macaddr.t
  ; src : Logs.src
  ; bstr_ic : Bstr.t
  ; bstr_oc : Bstr.t }
and 'a packet =
  { src : Macaddr.t option
  ; dst : Macaddr.t
  ; protocol : Packet.protocol
  ; payload : 'a }
and handler = Slice_bstr.t packet -> unit

let mac { mac; _ } = mac

let write_directly_into t (packet : (Bstr.t -> int) packet) =
  let fn = packet.payload in
  let src = Option.value ~default:t.mac packet.src in
  let pkt = { Packet.src; dst= packet.dst; protocol= Some packet.protocol } in
  Packet.encode_into pkt ~off:0 t.bstr_oc;
  let bstr = Bstr.sub t.bstr_oc ~off:14 ~len:(Bstr.length t.bstr_oc - 14) in
  let plus = fn bstr in
  Logs.debug ~src:t.src (fun m -> m "write ethernet packet src:%a -> dst:%a (%d byte(s))"
    Macaddr.pp src Macaddr.pp packet.dst plus);
  Logs.debug ~src:t.src (fun m -> m "@[<hov>%a@]"
    (Hxd_string.pp Hxd.default) (Bstr.sub_string t.bstr_oc ~off:0 ~len:(14 + plus)));
  Miou_solo5.Net.write_bigstring t.net ~off:0 ~len:(14 + plus) t.bstr_oc

let rec daemon t =
  let len = Miou_solo5.Net.read_bigstring t.net t.bstr_ic in
  begin match Packet.decode t.bstr_ic ~len with
  | Error _ ->
    let str = Bstr.sub_string t.bstr_ic ~off:0 ~len in
    Logs.err ~src:t.src (fun m -> m "Invalid Ethernet packet");
    Logs.err ~src:t.src (fun m -> m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) str)
  | Ok ({ Packet.protocol= Some protocol; src; dst }, payload) ->
    let packet =
      { src= Some src; dst; protocol; payload } in
    if Macaddr.compare dst t.mac == 0
    || Macaddr.is_unicast dst == false
    then
      try t.handler packet
      with exn ->
        Logs.err ~src:t.src (fun m -> m "Unexpected exception from the user's handler: %s"
          (Printexc.to_string exn));
    else begin
      let payload = Slice_bstr.to_string payload in
      Logs.debug ~src:t.src (fun m -> m "Ignore (%a -> %a):" Macaddr.pp src Macaddr.pp dst);
      Logs.debug ~src:t.src (fun m -> m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) payload);
    end
  | Ok _ -> () end;
  daemon t

let write_directly_into t ?src ~dst ~protocol fn =
  let pkt = { src; dst; protocol; payload= fn } in
  write_directly_into t pkt

let guard err fn = if fn () then Ok () else Error err

type daemon = unit Miou.t

let create ?(mtu= 1500) ?(handler= ignore) mac net =
  let ( let* ) = Result.bind in
  let* () = guard `MTU_too_small @@ fun () -> mtu > 14 in (* enough for Ethernet packets *)
  let bstr_ic = Bstr.create (14 + mtu) in
  let bstr_oc = Bstr.create (14 + mtu) in
  (* NOTE(dinosaure): the first [Bstr.sub] does a [malloc()], then any
     [Bstr.sub] are cheap. We should use [Slice] instead of [Bstr]. TODO! *)
  let bstr_ic = Bstr.sub bstr_ic ~off:0 ~len:(14 + mtu) in
  let bstr_oc = Bstr.sub bstr_oc ~off:0 ~len:(14 + mtu) in
  let src = Logs.Src.create (Macaddr.to_string mac) in
  let t =
    { net
    ; handler
    ; mtu
    ; mac
    ; src
    ; bstr_ic
    ; bstr_oc } in
  let daemon = Miou.async @@ fun () -> daemon t in
  Ok (daemon, t)

let _cnt = Atomic.make 0

let mtu { mtu; _ } = mtu
let macaddr { mac; _ } = mac

let set_handler t handler =
  Atomic.incr _cnt;
  t.handler <- handler;
  if Atomic.get _cnt > 1
  then Logs.warn ~src:t.src (fun m -> m "Ethernet handler modified more than once")

let kill = Miou.cancel
