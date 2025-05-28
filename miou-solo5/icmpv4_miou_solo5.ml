let src = Logs.Src.create "icmpv4"

module Log = (val Logs.src_log src : Logs.LOG)

module Packet = struct
  type 'a t =
    { code : int
    ; kind : 'a kind
    ; checksum : int
    ; shdr : 'a }
  and 'a kind =
    | Echo_reply : id_and_seq kind
    | Destination_unreachable : next_hop_mtu kind
    | Source_quench : unused kind
    | Redirect : Ipaddr.V4.t kind
    | Echo_request : id_and_seq kind
    | Time_exceeded : unused kind
    | Parameter_problem : pointer kind
    | Timestamp_request : id_and_seq kind
    | Timestamp_reply : id_and_seq kind
    | Information_request : id_and_seq kind
    | Information_reply : id_and_seq kind
  and id_and_seq = int * int
  and next_hop_mtu = Hop of int [@@unboxed]
  and pointer = Pointer of int [@@unboxed]
  and unused = Unused
  and pack = Kind : 'a kind -> pack
  and packet = Packet : 'a t -> packet

  open Bin

  let kind =
    let f = function
      | 0 -> Kind Echo_reply
      | 3 -> Kind Destination_unreachable
      | 4 -> Kind Source_quench
      | 5 -> Kind Redirect
      | 8 -> Kind Echo_request
      | 11 -> Kind Time_exceeded
      | 12 -> Kind Parameter_problem
      | 13 -> Kind Timestamp_request
      | 14 -> Kind Timestamp_reply
      | 15 -> Kind Information_request
      | 16 -> Kind Information_reply
      | _ -> invalid_arg "Invalid ICMPv4 message" in
    let g = function
      | Kind Echo_reply -> 0
      | Kind Destination_unreachable -> 3
      | Kind Source_quench -> 4
      | Kind Redirect -> 5
      | Kind Echo_request -> 8
      | Kind Time_exceeded -> 11
      | Kind Parameter_problem -> 12
      | Kind Timestamp_request -> 13
      | Kind Timestamp_reply -> 14
      | Kind Information_request -> 15
      | Kind Information_reply -> 16 in
    map uint8 f g

  let unused = const Unused
  let ipaddr = map beint32 Ipaddr.V4.of_int32 Ipaddr.V4.to_int32

  let id_and_seq =
    record (fun id seq -> (id, seq))
    |+ field beuint16 fst
    |+ field beuint16 snd
    |> sealr

  let next_hop_mtu =
    let f arr = Hop arr.(1) in
    let g (Hop mtu) = [| 0; mtu |] in
    map (seq ~len:2 beuint16) f g

  let pointer =
    let f byte = Pointer byte in
    let g (Pointer byte) = byte in
    map uint8 f g

  let shdr : type a. a kind -> a Bin.t = function
    | Echo_request -> id_and_seq
    | Echo_reply -> id_and_seq
    | Timestamp_request -> id_and_seq
    | Timestamp_reply -> id_and_seq
    | Information_request -> id_and_seq
    | Information_reply -> id_and_seq
    | Destination_unreachable -> next_hop_mtu
    | Time_exceeded -> unused
    | Source_quench -> unused
    | Redirect -> ipaddr
    | Parameter_problem -> pointer

  let t ~kind:knd =
    let fn _knd code checksum shdr =
      { kind= knd; code; checksum; shdr } in
    record fn
    |+ field kind (Fun.const (Kind knd))
    |+ field uint8 (fun t -> t.code)
    |+ field beuint16 (fun t -> t.checksum)
    |+ field (shdr knd) (fun t -> t.shdr)
    |> sealr

  let decode ?(off= 0) str =
    let pos = off in
    let Kind kind = decode kind str (ref off) in
    let off = ref off in
    let pkt = decode (t ~kind) str off in
    (* TODO(dinosaure): can we avoid this copy? *)
    let buf = Bytes.of_string str in
    let len = String.length str in
    Bytes.set_uint16_be buf (pos + 2) 0;
    let chk = Utcp.Checksum.digest_string ~off:pos ~len (Bytes.unsafe_to_string buf) in
    Log.debug (fun m -> m "checksum: %04x, has: %04x" pkt.checksum chk);
    if pkt.checksum != chk
    then invalid_arg "Invalid ICMPv4 checksum";
    let payload = String.sub str !off (String.length str - !off) in
    Packet pkt, payload

  let decode ?off bstr =
    try Ok (decode ?off bstr)
    with exn ->
      Log.err (fun m -> m "Got an exception while decoding ICMPv4 packet: %s"
        (Printexc.to_string exn));
      Error `Invalid_ICMPv4_packet

  let to_bytes pkt =
    Bin.to_string (t ~kind:pkt.kind) pkt
    |> Bytes.unsafe_of_string
end

module IPv4 = Ipv4_miou_solo5

let input ipv4 pkt payload =
  let dst = pkt.IPv4.src in
  match Packet.decode payload with
  | Error _ ->
      Logs.err (fun m -> m "Invalid ICMPv4 packet:");
      Logs.err (fun m -> m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) payload)
  | Ok (Packet pkt, payload) ->
      begin match pkt.kind with
      | Packet.Echo_request ->
          let pkt = { Packet.code= 0; kind= Echo_reply; checksum= 0; shdr= pkt.shdr } in
          let buf = Packet.to_bytes pkt in
          let chk = Utcp.Checksum.digest_strings [ Bytes.unsafe_to_string buf; payload ] in
          Bytes.set_uint16_be buf 2 chk;
          let pkt = Bytes.unsafe_to_string buf in
          let pkt = IPv4.Writer.of_strings ipv4 [ pkt; payload ] in
          let result = IPv4.write ipv4 dst ~protocol:1 pkt in
          let err _ =
            Log.err (fun m -> m "Impossible to send ICMPv4 echo-reply packet") in
          let _ = Result.map_error err result in ()
      | _ ->
          Log.debug (fun m -> m "Ignore ICMPv4 packet") end

type t =
  { mutex : Miou.Mutex.t
  ; condition : Miou.Condition.t
  ; queue : (IPv4.packet * string) Queue.t
  ; ipv4 : IPv4.t
  ; orphans : unit Miou.orphans }

let rec clean orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      match Miou.await prm with
      | Ok () -> clean orphans
      | Error exn ->
          Logs.err (fun m -> m "Unexpected exception from an ICMPv4 task: %s"
            (Printexc.to_string exn));
          clean orphans

let rec handler t =
  clean t.orphans;
  let todo = Miou.Mutex.protect t.mutex @@ fun () ->
    while Queue.is_empty t.queue
    do Miou.Condition.wait t.condition t.mutex done;
    let todo = Queue.create () in
    Queue.transfer t.queue todo; todo in
  let fn (pkt, payload) =
    ignore (Miou.async ~orphans:t.orphans @@ fun () -> input t.ipv4 pkt payload) in
  Queue.iter fn todo;
  handler t

type daemon = unit Miou.t * t

let handler ipv4 : daemon =
  let mutex = Miou.Mutex.create () in
  let condition = Miou.Condition.create () in
  let queue = Queue.create () in
  let orphans = Miou.orphans () in
  let t = { mutex; condition; queue; ipv4; orphans } in
  Miou.async (fun () -> handler t), t

let kill (prm, _) = Miou.cancel prm

let transfer (_, t) (pkt, payload) =
  let payload = match payload with
    | IPv4.Slice bstr -> Slice_bstr.to_string bstr
    | IPv4.String str -> str in
  Miou.Mutex.protect t.mutex @@ fun () ->
  Queue.push (pkt, payload) t.queue;
  Miou.Condition.signal t.condition
