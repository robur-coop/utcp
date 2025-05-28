module Ethernet = Ethernet_miou_solo5

[@@@warning "-37"]

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Packet = struct
  type t =
    { operation : operation
    ; src_mac : Macaddr.t
    ; dst_mac : Macaddr.t
    ; src_ip : Ipaddr.V4.t
    ; dst_ip : Ipaddr.V4.t }
  and operation =
    | Request | Reply

  let operation_of_int = function
    | 1 -> Ok Request
    | 2 -> Ok Reply
    | n -> error_msgf "Invalid ARPv4 operation (%02x)" n

  let operation_to_int = function
    | Request -> 1
    | Reply -> 2

  let guard err fn = if fn () then Ok () else Error err

  let decode ?(off= 0) str =
    let ( let* ) = Result.bind in
    let* () = guard `Invalid_ARPv4_packet @@ fun () -> String.length str - off >= 28 in
    let operation = String.get_uint16_be str (off + 6) in
    let* operation = operation_of_int operation in
    let src_mac = Macaddr.of_octets_exn (String.sub str (off + 8) 6) in
    let src_ip = Ipaddr.V4.of_int32 (String.get_int32_be str (off + 14)) in
    let dst_mac = Macaddr.of_octets_exn (String.sub str (off + 18) 6) in
    let dst_ip = Ipaddr.V4.of_int32 (String.get_int32_be str (off + 24)) in
    Ok { operation; src_mac; dst_mac; src_ip; dst_ip }

  let unsafe_encode_into t ?(off= 0) bstr =
    Bstr.set_uint16_be bstr (off + 0) 1;
    Bstr.set_uint16_be bstr (off + 2) 0x0800;
    Bstr.set_uint8 bstr (off + 4) 6;
    Bstr.set_uint8 bstr (off + 5) 4;
    Bstr.set_uint16_be bstr (off + 6) (operation_to_int t.operation);
    let src_mac = Macaddr.to_octets t.src_mac in
    Bstr.blit_from_string src_mac ~src_off:0 bstr ~dst_off:(off + 8) ~len:6;
    Bstr.set_int32_be bstr (off + 14) (Ipaddr.V4.to_int32 t.src_ip);
    let dst_mac = Macaddr.to_octets t.dst_mac in
    Bstr.blit_from_string dst_mac ~src_off:0 bstr ~dst_off:(off + 18) ~len:6;
    Bstr.set_int32_be bstr (off + 24) (Ipaddr.V4.to_int32 t.dst_ip);
    28
end

let mac0 = Macaddr.of_octets_exn (String.make 6 '\000')

type w = Macaddr.t Miou.Computation.t

type entry =
  | Static of Macaddr.t * bool
  | Dynamic of Macaddr.t * int
  | Pending of w * int

type t =
  { cache : (Ipaddr.V4.t, entry) Hashtbl.t
  ; macaddr : Macaddr.t
  ; ipaddr : Ipaddr.V4.t
  ; timeout : int
  ; retries : int
  ; mutable epoch : int
  ; src : Logs.src
  ; eth : Ethernet.t
  ; mutex : Miou.Mutex.t
  ; condition : Miou.Condition.t
  ; queue : string Ethernet.packet Queue.t }

let alias t ipaddr =
  let () = match Hashtbl.find t.cache ipaddr with
    | exception Not_found -> ()
    | Pending (c, _) -> ignore (Miou.Computation.try_return c t.macaddr)
    | _ -> () in
  Hashtbl.replace t.cache ipaddr (Static (t.macaddr, true));
  let pkt =
    { Packet.operation= Packet.Request
    ; src_mac= t.macaddr
    ; dst_mac= mac0
    ; src_ip= ipaddr
    ; dst_ip= ipaddr } in
  (pkt, Macaddr.broadcast)

let write t (arp, dst) =
  (* NOTE(dinosaure): we already check, in [create] that the MTU is more than
     [28] bytes. The buffer given by [Ethernet] is also more than [28] bytes. *)
  let fn = Packet.unsafe_encode_into arp ~off:0 in
  Ethernet.write_directly_into t.eth ~dst ~protocol:Ethernet.ARPv4 fn

let guard err fn = if fn () then Ok () else Error err
let macaddr t = t.macaddr

let request t dst_ip =
  let dst_mac = Macaddr.broadcast in
  { Packet.operation= Request
  ; src_mac= t.macaddr
  ; dst_mac
  ; src_ip= t.ipaddr
  ; dst_ip }, dst_mac

let reply arp macaddr =
  let pkt =
    { Packet.operation= Packet.Reply
    ; src_mac= macaddr
    ; dst_mac= arp.Packet.src_mac
    ; src_ip= arp.Packet.dst_ip
    ; dst_ip= arp.Packet.src_ip } in
  pkt, arp.Packet.src_mac

exception Timeout
exception Clear

let empty_bt = Printexc.get_callstack 0
let timeout = (Timeout, empty_bt)
let timeout c = ignore (Miou.Computation.try_cancel c timeout)
let clear = (Clear, empty_bt)
let clear c = ignore (Miou.Computation.try_cancel c clear)

let tick t =
  let epoch = t.epoch in
  let fn k v (pkts, to_remove, timeouts) = match v with
    | Dynamic (_, tick) when tick == epoch ->
        pkts, (k :: to_remove), timeouts
    | Dynamic (_, tick) when tick == epoch + 1 ->
        request t k :: pkts, to_remove, timeouts
    | Pending (w, retry) when retry == epoch ->
        pkts, k :: to_remove, w :: timeouts
    | Pending _ ->
        request t k :: pkts, to_remove, timeouts
    | _ -> pkts, to_remove, timeouts in
  let outs, to_remove, timeouts = Hashtbl.fold fn t.cache ([], [], []) in
  List.iter (Hashtbl.remove t.cache) to_remove;
  List.iter (write t) outs;
  List.iter timeout timeouts;
  t.epoch <- t.epoch + 1

let handle_request t arp =
  let dst = arp.Packet.dst_ip in
  let src = arp.Packet.src_ip in
  let src_mac = arp.Packet.src_mac in
  Logs.debug ~src:t.src (fun m -> m "%a:%a: who has %a?"
    Macaddr.pp src_mac Ipaddr.V4.pp src Ipaddr.V4.pp dst);
  match Hashtbl.find t.cache dst with
  | exception Not_found -> ()
  | Static (macaddr, true) ->
      write t (reply arp macaddr)
  | _ -> ()

let handle_reply t src macaddr =
  let entry = Dynamic (macaddr, t.epoch + t.timeout) in
  Logs.debug ~src:t.src (fun m -> m "handle ARPv4 reply packet from %a:%a"
    Macaddr.pp macaddr Ipaddr.V4.pp src);
  match Hashtbl.find t.cache src with
  | exception Not_found -> ()
  | Static (_, adv) ->
      if adv && Macaddr.compare macaddr mac0 == 0
      then Logs.debug ~src:t.src (fun m -> m "ignoring gratuitious ARP from %a using %a"
        Macaddr.pp macaddr Ipaddr.V4.pp src)
  | Dynamic (macaddr', _) ->
      if Macaddr.compare macaddr macaddr' != 0
      then Logs.debug ~src:t.src (fun m -> m "set %a from %a to %a"
        Ipaddr.V4.pp src Macaddr.pp macaddr' Macaddr.pp macaddr);
      Hashtbl.replace t.cache src entry
  | Pending (c, _) ->
      Logs.debug ~src:t.src (fun m -> m "%a is-at %a"
        Ipaddr.V4.pp src Macaddr.pp macaddr);
      ignore (Miou.Computation.try_return c macaddr);
      Hashtbl.replace t.cache src entry

let input t pkt =
  match Packet.decode pkt.Ethernet.payload with
  | Error _ ->
      Logs.err ~src:t.src (fun m -> m "Invalid ARPv4 packet:");
      Logs.err ~src:t.src (fun m -> m "@[<hov>%a@]" (Hxd_string.pp Hxd.default)
        pkt.Ethernet.payload)
  | Ok arp ->
      if Ipaddr.V4.compare arp.Packet.src_ip arp.Packet.dst_ip == 0
      || arp.Packet.operation == Packet.Reply
      then
        let mac = arp.Packet.src_mac
        and src = arp.Packet.src_ip in
        handle_reply t src mac
      else handle_request t arp

let to_error (exn, _bt) = match exn with
  | Timeout -> `Timeout
  | Clear -> `Clear
  | exn -> `Exn exn

type error =
  [ `Timeout
  | `Clear
  | `Exn of exn ]

let pp_error ppf = function
  | `Timeout -> Fmt.string ppf "Timeout"
  | `Clear -> Fmt.string ppf "ARP table reset"
  | `Exn exn -> Fmt.pf ppf "Unexpected exception: %s" (Printexc.to_string exn)

let query t ipaddr =
  match Hashtbl.find t.cache ipaddr with
  | exception Not_found ->
      let w = Miou.Computation.create () in
      let pending = Pending (w, t.epoch + t.retries) in
      Hashtbl.replace t.cache ipaddr pending;
      write t (request t ipaddr);
      Miou.Computation.await w
      |> Result.map_error to_error
  | Pending (w, _) ->
      Miou.Computation.await w
      |> Result.map_error to_error
  | Static (macaddr, _)
  | Dynamic (macaddr, _) -> Ok macaddr

let ips t =
  let fn k v acc = match v with
    | Static (_, true) -> k :: acc
    | _ -> acc in
  Hashtbl.fold fn t.cache []

let add_ip t ipaddr =
  match ips t with
  | [] ->
      Hashtbl.iter (fun _ -> function
        | Pending (w, _) -> clear w
        | _ -> ()) t.cache;
      Hashtbl.clear t.cache;
      write t (alias t ipaddr)
  | _ ->
      write t (alias t ipaddr)

let set_ips t = function
  | [] ->
      Hashtbl.iter (fun _ -> function
        | Pending (w, _) -> clear w
        | _ -> ()) t.cache;
      Hashtbl.clear t.cache
  | ipaddr :: rest ->
      Hashtbl.iter (fun _ -> function
        | Pending (w, _) -> clear w
        | _ -> ()) t.cache;
      Hashtbl.clear t.cache;
      write t (alias t ipaddr);
      List.iter (add_ip t) rest

type daemon = unit Miou.t

type event =
  | In of string Ethernet.packet Queue.t
  | Tick

let read_or_sync ?(delay= 1_500_000_000) t =
  let prm1 = Miou.async @@ fun () ->
    Miou.Mutex.protect t.mutex @@ fun () ->
    if Queue.is_empty t.queue
    then Miou.Condition.wait t.condition t.mutex;
    let todo = Queue.create () in
    Queue.transfer t.queue todo; In todo in
  let prm0 = Miou.async @@ fun () ->
    Miou_solo5.sleep delay;
    Tick in
  match Miou.await_first [ prm0; prm1 ] with
  | Ok value -> value
  | Error exn ->
      Logs.err ~src:t.src (fun m -> m "Unexpected exception: %s"
        (Printexc.to_string exn));
      In (Queue.create ())

let arp ?(delay= 1_500_000_000) t =
  let rec go rem =
    let t0 = Miou_solo5.clock_monotonic () in
    match read_or_sync ~delay:rem t with
    | In queue ->
      let fn = input t in
      Queue.iter fn queue;
      let t1 = Miou_solo5.clock_monotonic () in
      let rem = rem - (t1 - t0) in
      let rem = if rem <= 0 then delay else rem in
      go rem
    | Tick -> tick t; go delay in
  go delay

let create ?(delay= 1_500_000_000) ?(timeout= 800) ?(retries= 5) ?src ?ipaddr eth =
  let ( let* ) = Result.bind in
  let macaddr = Ethernet.macaddr eth in
  (* enough for ARP packets *)
  let* () = guard `MTU_too_small @@ fun () -> Ethernet.mtu eth >= 28 in
  let src = match src with
    | None -> Logs.Src.create (Fmt.str "%a" Macaddr.pp macaddr)
    | Some src -> src in
  if timeout <= 0
  then Fmt.invalid_arg "Arp_miou_solo5.create: null or negative timeout";
  if retries < 0
  then Fmt.invalid_arg "Arg_miou_solo5.create: negative retries value";
  let unknown = Option.is_none ipaddr in
  let ipaddr = Option.value ~default:Ipaddr.V4.any ipaddr in
  let cache = Hashtbl.create 0x10 in
  let t = { cache; macaddr; ipaddr; timeout; retries; epoch= 0; src; eth
          ; mutex= Miou.Mutex.create ()
          ; condition= Miou.Condition.create ()
          ; queue= Queue.create () } in
  if unknown == false
  then write t (alias t ipaddr);
  let prm = Miou.async (fun () -> arp ~delay t) in
  Ok (prm, t)

let transfer t pkt =
  let payload = Slice_bstr.sub_string pkt.Ethernet.payload ~off:0 ~len:28 in
  let pkt = { pkt with Ethernet.payload } in
  Miou.Mutex.protect t.mutex @@ fun () ->
  Queue.push pkt t.queue;
  Miou.Condition.signal t.condition

let kill = Miou.cancel
