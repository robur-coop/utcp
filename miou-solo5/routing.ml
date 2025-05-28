module ARPv4 = Arp_miou_solo5

let broadcast = Ok Macaddr.broadcast

let macaddr_of_multicast ipaddr =
  let buf = Ipaddr.V4.to_octets ipaddr in
  let mac = Bytes.create 6 in
  Bytes.set_uint8 mac 0 0x01;
  Bytes.set_uint8 mac 1 0x00;
  Bytes.set_uint8 mac 2 0x5e;
  Bytes.set_uint8 mac 3 (String.get_uint8 buf 1 land 0x7f);
  Bytes.set_uint8 mac 4 (String.get_uint8 buf 2);
  Bytes.set_uint8 mac 5 (String.get_uint8 buf 3);
  Macaddr.of_octets_exn (Bytes.unsafe_to_string mac)

let destination_macaddr network gateway arp ipaddr =
  if Ipaddr.V4.(compare broadcast) ipaddr == 0
  || Ipaddr.V4.(compare any) ipaddr == 0
  || Ipaddr.V4.(compare (Prefix.broadcast network)) ipaddr == 0
  then broadcast
  else if Ipaddr.V4.is_multicast ipaddr
  then Ok (macaddr_of_multicast ipaddr)
  else if Ipaddr.V4.Prefix.mem ipaddr network
  then ARPv4.query arp ipaddr
  else
    match gateway with
    | None -> Error `Gateway
    | Some gateway ->
        ARPv4.query arp gateway
