module Ethernet = Ethernet_miou_solo5
module ARPv4 = Arp_miou_solo5
module IPv4 = Ipv4_miou_solo5
module ICMPv4 = Icmpv4_miou_solo5
module UDPv4 = Udpv4_miou_solo5

exception Net_unreach
exception Closed_by_peer
exception Connection_refused

module Buffer = struct
  type t =
    { mutable bstr : Bstr.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let bstr = Bstr.create size in
    { bstr; off= 0; len= 0 }

  let available { bstr; off; len; } =
    Bstr.length bstr - off - len

  let length { len; _ } = len

  let compress t =
    if t.len == 0 then begin
      t.off <- 0;
      t.len <- 0
    end else if t.off > 0 then begin
      Bstr.blit t.bstr ~src_off:t.off t.bstr ~dst_off:0 ~len:t.len;
      t.off <- 0;
    end

  let get t ~fn =
    let n = fn t.bstr ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len == 0 then t.off <- 0;
    n

  let put t ~fn =
    compress t;
    let off = t.off + t.len in
    let bstr = t.bstr in
    if Bstr.length bstr == t.len then begin
      t.bstr <- Bstr.create (2 * Bstr.length bstr);
      Bstr.blit bstr ~src_off:t.off t.bstr ~dst_off:0 ~len:t.len
    end;
    let n = fn t.bstr ~off ~len:(Bstr.length t.bstr - off) in
    t.len <- t.len + n;
    n
end

module Notify = struct
  type 'a t =
    { queue : 'a Queue.t
    ; mutex : Miou.Mutex.t
    ; condition : Miou.Condition.t }

  let create () =
    { queue= Queue.create ()
    ; mutex= Miou.Mutex.create ()
    ; condition= Miou.Condition.create () }

  let signal value t =
    (* Miou.Mutex.protect t.mutex @@ fun () -> *)
    Queue.push value t.queue;
    Miou.Condition.signal t.condition

  let await t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    while Queue.is_empty t.queue do
      Miou.Condition.wait t.condition t.mutex
    done;
    Queue.pop t.queue
end

(* NOTE(dinosaure): μTCP is actually well-abstracted about IPv4 and IPv6 but we
   only have IPv4 implementation. We should handle easily the IPv6 at this
   layer I think. *)
module TCPv4 = struct
  let src = Logs.Src.create "tcpv4"

  module Log = (val Logs.src_log src : Logs.LOG)

  type w = (unit, [ `Eof | `Msg of string ]) result Notify.t

  type state =
    { mutable tcp : w Utcp.state
    ; ipv4 : IPv4.t
    ; udpv4 : UDPv4.state
    ; queue : Utcp.output Queue.t
    ; mutex : Miou.Mutex.t
    ; condition : Miou.Condition.t
    ; accept : (int, accept) Hashtbl.t }

  and accept =
    | Await of flow Miou.Computation.t
    | Pending of flow Queue.t

  and flow =
    { state : state
    ; src : Logs.src
    ; flow : Utcp.flow
    ; buffer : Buffer.t
    ; mutable closed : bool }

  let[@inline] now () =
    let n = Miou_solo5.clock_monotonic () in
    Mtime.of_uint64_ns (Int64.of_int n)

  let write_ipv4 ipv4 (src, dst, seg) =
    let len = Utcp.Segment.length seg in
    Log.debug (fun m -> m "write-out %d byte(s) to %a" (20 (* ipv4 packet *) + len) Ipaddr.V4.pp dst);
    let fn bstr =
      let cs = Cstruct.of_bigarray bstr in
      let src = Ipaddr.V4 src
      and dst = Ipaddr.V4 dst in
      Utcp.Segment.encode_and_checksum_into (now ()) cs ~src ~dst seg in
    let pkt = IPv4.Writer.into ipv4 ~len fn in
    match IPv4.write ipv4 ~src dst ~protocol:6 pkt with
    | Ok () -> ()
    | Error `Route_not_found ->
        Log.err (fun m -> m "%a is unreachable" Ipaddr.V4.pp dst);
        raise Net_unreach

  let write_ip ipv4 (src, dst, seg) =
    match src, dst with
    | Ipaddr.V4 src, Ipaddr.V4 dst -> write_ipv4 ipv4 (src, dst, seg)
    | _ -> failwith "IPv6 not implemented"

  type result =
    | Filled
    | Eof
    | Refused

  let fill t data =
    let rec go data =
      let { Cstruct.buffer; off; len } = data in
      if len > 0 then begin
        let len = Int.min len (Buffer.available t.buffer) in
        let fn dst ~off:dst_off ~len:_ =
          Bstr.blit buffer ~src_off:off dst ~dst_off ~len; len in
        let _ = Buffer.put t.buffer ~fn in
        go (Cstruct.shift data len)
      end in
    go data; Filled

  let read_into t =
    match Utcp.recv t.state.tcp (now ()) t.flow with
    | Ok (tcp, data, c, segs) ->
        t.state.tcp <- tcp;
        List.iter (write_ip t.state.ipv4) segs;
        Logs.debug ~src:t.src (fun m -> m "recv new packet (%d byte(s))"
          (Cstruct.length data));
        if Cstruct.length data == 0
        then match Notify.await c with
        | Ok () ->
            begin match Utcp.recv t.state.tcp (now ()) t.flow with
            | Ok (tcp, data, _c, segs) ->
                (* TODO(dinosaure): assert (c == _c)? *)
                t.state.tcp <- tcp;
                List.iter (write_ip t.state.ipv4) segs;
                Logs.debug ~src:t.src (fun m -> m "recv new packet (%d byte(s)) (second)"
                  (Cstruct.length data));
                if Cstruct.length data == 0
                then Eof else fill t data
            | Error `Not_found -> Refused
            | Error `Eof -> Eof
            | Error (`Msg msg) ->
                Logs.err ~src:t.src (fun m -> m "%a error while read (second recv): %s"
                  Utcp.pp_flow t.flow msg);
                Refused end
        | Error `Eof -> Eof
        | Error (`Msg msg) ->
            Logs.err ~src:t.src (fun m -> m "%a error from computation while recv: %s"
              Utcp.pp_flow t.flow msg);
            Refused
        else fill t data
    | Error `Eof -> Eof
    | Error (`Msg msg) ->
        Logs.err ~src:t.src (fun m -> m "%a error while read: %s"
          Utcp.pp_flow t.flow msg);
        Refused
    | Error `Not_found -> Refused

  let read t ?off:(dst_off= 0) ?len buf =
    if t.closed then 0
    else
      let len = match len with
        | Some len -> len
        | None -> Bytes.length buf - dst_off in
      let fn bstr ~off:src_off ~len:src_len =
        let len = Int.min src_len len in
        Bstr.blit_to_bytes bstr ~src_off buf ~dst_off ~len; len in
      if Buffer.length t.buffer > 0
      then Buffer.get t.buffer ~fn
      else match read_into t with
      | Filled -> Buffer.get t.buffer ~fn
      | Eof ->
          Logs.debug ~src:t.src (fun m -> m "End-of-transmision received");
          0
      | Refused ->
          Logs.err ~src:t.src (fun m -> m "Connection refused");
          t.closed <- true; 0

  let rec really_read t off len buf =
    let len' = read t ~off ~len buf in
    if len' == 0 then raise End_of_file
    else if len - len' > 0 then
      really_read t (off + len') (len - len') buf

  let really_read t ?(off= 0) ?len buf =
    let len = match len with None -> Bytes.length buf - off | Some len -> len in
    if off < 0 || len < 0 || off > Bytes.length buf - len
    then invalid_arg "TCPv4.really_read";
    if len > 0 then really_read t off len buf

  (* NOTE(dinosaure): μTCP takes the ownership on [cs], so we can not use a
     internal buffer associated to our flow to avoid allocation-per-writing. The
     only viable solution seems to modify μTCP to use strings instead of
     [Cstruct.t]... *)
  let rec write t cs =
    match Utcp.send t.state.tcp (now ()) t.flow cs with
    | Error `Not_found -> raise Connection_refused
    | Error (`Msg msg) ->
        Logs.err ~src:t.src (fun m -> m "%a error while write: %s"
          Utcp.pp_flow t.flow msg);
        raise Closed_by_peer
    | Ok (tcp, bytes_sent, c, segs) ->
        t.state.tcp <- tcp;
        List.iter (write_ip t.state.ipv4) segs;
        Logs.debug ~src:t.src (fun m -> m "write %d byte(s)" bytes_sent);
        if bytes_sent < Cstruct.length cs
        then
          let result = Notify.await c in
          match result with
          | Error `Eof -> raise Closed_by_peer
          | Error (`Msg msg) ->
              Logs.err ~src:t.src (fun m -> m "%a error from condition while sending: %s"
                Utcp.pp_flow t.flow msg);
              raise Closed_by_peer
          | Ok () ->
              let cs = Cstruct.shift cs bytes_sent in
              if Cstruct.length cs > 0 then write t (Cstruct.shift cs bytes_sent)
              else Logs.debug ~src:t.src (fun m -> m "fully write the given string to peer")

  let write t ?(off= 0) ?len str =
    let len = match len with
      | Some len -> len
      | None -> String.length str - off in
    write t (Cstruct.of_string ~off ~len str)

  let close t =
    if t.closed then Fmt.invalid_arg "Connection already closed";
    match Utcp.close t.state.tcp (now ()) t.flow with
    | Ok (tcp, segs) ->
        t.state.tcp <- tcp;
        List.iter (write_ip t.state.ipv4) segs;
        t.closed <- true
    | Error `Not_found -> ()
    | Error (`Msg msg) ->
        Logs.err ~src:t.src (fun m -> m "%a error in close: %s"
          Utcp.pp_flow t.flow msg)

  let peers { flow; _ } = Utcp.peers flow

  let _eof = Error `Eof
  let _ok = Ok ()

  let handler state (pkt, payload) =
    let src = Ipaddr.V4 pkt.IPv4.src in
    let dst = Ipaddr.V4 pkt.IPv4.dst in
    Log.debug (fun m -> m "New TCPv4 packet (%a -> %a)" Ipaddr.pp src Ipaddr.pp dst);
    (* NOTE(dinosaure): μTCP takes the ownership on [cs] also. We can try to
       think, a bit deeply, about a zero-copy which includes the TCP layer if
       the given packet is not a part of a _segment_ but it requires some work
       on the μTCP side. Also, μTCP works with [mirage-tcpip] because
       [mirage-net-solo5] copies frames — which is not the case here! At least,
       we make the copy as far as possible.

       RE-NOTE(dinosaure): the viewer can say that we also do the copy for
       ARPv4 and ICMPv4 but they are not a part of our "happy-path". What we
       want to improve is the TCP/IP stack. ARPv4 & ICMPv4 are just side
       protocols. *)
    let cs = match payload with
      | IPv4.Slice slice ->
          let { Slice.buf; off; len; } = slice in
          let bstr = Bstr.copy buf in
          Cstruct.of_bigarray ~off ~len bstr
      | IPv4.String str -> Cstruct.of_string str in
    let tcp, ev, segs = Utcp.handle_buf state.tcp (now ()) ~src ~dst cs in
    state.tcp <- tcp;
    let none = ()
    and some = function
      | `Established (flow, None) ->
          let (_, src_port), (ipaddr, port) = Utcp.peers flow in
          Log.debug (fun m -> m "established connection with %a:%d"
            Ipaddr.pp ipaddr port);
          let src = Logs.Src.create (Fmt.str "%a:%d" Ipaddr.pp ipaddr port) in
          let buffer = Buffer.create 0x7ff in
          let flow = { state; src; flow; buffer; closed= false } in
          begin match Hashtbl.find state.accept src_port with
          | Await c ->
              Hashtbl.remove state.accept src_port;
              Log.debug (fun m -> m "transmit the new incoming TCPv4 connection to the handler");
              ignore (Miou.Computation.try_return c flow)
          | Pending q -> Queue.push flow q
          | exception Not_found ->
              let q = Queue.create () in
              Queue.push flow q;
              Hashtbl.add state.accept src_port (Pending q) end
      | `Established (flow, Some c) ->
          Log.debug (fun m -> m "connection established (%a)" Utcp.pp_flow flow);
          Notify.signal _ok c
      | `Drop (flow, c, cs) ->
          Log.debug (fun m -> m "drop (%a)" Utcp.pp_flow flow);
          List.iter (Notify.signal _eof) cs;
          Option.iter (Notify.signal _ok) c
      | `Signal (flow, cs) ->
          Log.debug (fun m -> m "signal (%a)(%d)" Utcp.pp_flow flow (List.length cs));
          List.iter (Notify.signal _ok) cs in
    Option.fold ~none ~some ev;
    Logs.debug (fun m -> m "%d segment(s) produced" (List.length segs));
    List.iter (fun out -> Queue.push out state.queue) segs

  let rec transfer state acc = match Queue.pop state.queue with
    | exception Queue.Empty -> acc
    | out -> transfer state (out :: acc)

  let rec daemon state n =
    let handler's_outs = transfer state [] in
    let tcp, drops, outs = Utcp.timer state.tcp (now ()) in
    state.tcp <- tcp;
    let outs = List.rev_append handler's_outs outs in
    let fn out =
      Log.debug (fun m -> m "write new TCPv4 packet from daemon");
      try write_ip state.ipv4 out
      with
      | Net_unreach ->
        let (_, dst, _) = out in
        Log.err (fun m -> m "Network unreachable for %a" Ipaddr.pp dst)
      | exn ->
        let (src, dst, _) = out in
        Log.err (fun m -> m "Unexpected exception (%a -> %a): %s"
          Ipaddr.pp src Ipaddr.pp dst (Printexc.to_string exn)) in
    List.iter fn outs;
    let fn (_id, err, rcv, snd) =
      let err = match err with
        | `Retransmission_exceeded -> `Msg "retransmission exceeded"
        | `Timer_2msl -> `Eof
        | `Timer_connection_established -> `Eof
        | `Timer_fin_wait_2 -> `Eof in
      let err = Error err in
      Notify.signal err rcv;
      Notify.signal err snd in
    List.iter fn drops;
    Miou_solo5.sleep 100_000_000;
    daemon state (n+1)

  type listen = Listen of int [@@unboxed]

  let accept state (Listen port) =
    match Hashtbl.find state.accept port with
    | exception Not_found ->
        let c = Miou.Computation.create () in
        Hashtbl.add state.accept port (Await c);
        Miou.Computation.await_exn c
    | Await c ->
        Log.debug (fun m -> m "listen on %a:%d: multiple waiters"
          Ipaddr.V4.pp (IPv4.src state.ipv4) port);
        Miou.Computation.await_exn c
    | Pending q -> match Queue.pop q with
      | exception Queue.Empty ->
          let c = Miou.Computation.create () in
          Hashtbl.replace state.accept port (Await c);
          Miou.Computation.await_exn c
      | flow -> flow

  let listen state port =
    let tcp = Utcp.start_listen state.tcp port in
    state.tcp <- tcp;
    Listen port

  type daemon = unit Miou.t

  let create ~name ipv4 =
    let tcp = Utcp.empty Notify.create name Mirage_crypto_rng.generate in
    let mutex = Miou.Mutex.create () in
    let condition = Miou.Condition.create () in
    let accept = Hashtbl.create 0x10 in
    let udpv4 = UDPv4.create ipv4 in
    let state = { tcp; ipv4; udpv4; queue= Queue.create (); mutex; condition; accept } in
    let prm = Miou.async (fun () -> daemon state 0) in
    prm, state

  let kill = Miou.cancel

  let connect state (dst, dst_port) =
    let src = Ipaddr.V4 (IPv4.src state.ipv4) in
    let dst = Ipaddr.V4 dst in
    let tcp, flow, c, seg = Utcp.connect ~src ~dst ~dst_port state.tcp (now ()) in
    let src = Logs.Src.create (Fmt.str "%a:%d" Ipaddr.pp dst dst_port) in
    state.tcp <- tcp;
    write_ip state.ipv4 seg;
    Logs.debug ~src (fun m -> m "Waiting for a TCP handshake");
    match Notify.await c with
    | Ok () ->
        let buffer = Buffer.create 0x7ff in
        { state; flow; src; buffer; closed= false }
    | Error `Eof ->
        Logs.err ~src (fun m -> m "%a error established connection (timeout)" Utcp.pp_flow flow);
        raise Connection_refused
    | Error (`Msg msg) ->
        Logs.err ~src (fun m -> m "%a error established connection: %s" Utcp.pp_flow flow msg);
        raise Connection_refused

  let udpv4 { udpv4; _ } = udpv4
end

let pp_error ppf = function
  | `MTU_too_small -> Fmt.string ppf "MTU too small"
  | `Exn exn -> Fmt.pf ppf "exception: %s" (Printexc.to_string exn)

let ethernet_handler arpv4 ipv4 = ();
  (* NOTE(dinosaure): about the handler and the packet received. The latter is
     in the form of a [Bstr.t] that physically corresponds to the [Bstr.t] used
     by Solo5. So to speak, from Solo5 to this [handler], there is no copy
     and the [Bstr.t] given is exclusive to the current task. However, as soon
     as this task is finished or would like to interact with the scheduler (and
     produce an effect), the guarantee of exclusivity is no longer assured.

     Depending on what you want to do, it may or may not be necessary to make a
     copy of this [Bstr.t].

     Currently, we can recognize a "happy-path" focus on TCP/IP. That is to say
     that upon receipt of a TCP/IP packet, we would like to go as far as
     possible without interruptions (like [Miou.yield] or any effects) to the
     TCP layer. In this case, the IPv4 handler has no effect, it just tries to
     reassemble packets it can depending on whether they are fragmented or not.
     In short, the happy path corresponds to the moment when IPv4 returns a
     packet in the form of a [Bstr.t] — that is to say that we have just
     obtained a non-fragmented and complete packet and this packet still
     physically corresponds to the one written by Solo5. Otherwise, [IPv4]
     returns a packet in the form of a [string] which means that it has been
     fragmented.

     This choice to return 2 types of payloads corresponds to a simple split: it
     is expensive to copy a [Bstr.t]. If we were to copy the [Bstr.t] given by
     [Ethernet], it is always more worthwhile to finally transform it into a
     [string] rather than "pretending that we still have a [Bstr.t]" underneath.
     This distinction also clarifies another point: ownership. If you are
     manipulating a [Bstr.t], you have to pay close attention to ownership and
     always consider this "happy path" (without interruptions). Otherwise, you
     can just manipulate the strings without asking yourself this question. *)
  let handler pkt =
    match pkt.Ethernet.protocol with
    | Ethernet.ARPv4 -> ARPv4.transfer arpv4 pkt
    | Ethernet.IPv4 -> IPv4.input ipv4 pkt
    | _ -> () in
  handler

let ipv4_handler icmpv4 udpv4 tcpv4 = (); fun ((hdr, _) as pkt) ->
  match hdr.IPv4.protocol with
  | 1 -> ICMPv4.transfer icmpv4 pkt
  | 6 -> TCPv4.handler tcpv4 pkt
  | 17 -> UDPv4.handler udpv4 pkt
  | _ -> ()

type tcpv4 =
  { ethernet_daemon : Ethernet.daemon
  ; arpv4_daemon : ARPv4.daemon
  ; icmpv4 : ICMPv4.daemon
  ; udpv4 : UDPv4.state
  ; tcpv4_daemon : TCPv4.daemon }

let kill t =
  TCPv4.kill t.tcpv4_daemon;
  ICMPv4.kill t.icmpv4;
  ARPv4.kill t.arpv4_daemon;
  Ethernet.kill t.ethernet_daemon

let tcpv4 ~name ?gateway cidr =
  let fn (net, cfg) () =
    let connect mac =
      let ( let* ) = Result.bind in
      let* daemon, eth = Ethernet.create ~mtu:cfg.Miou_solo5.Net.mtu mac net in
      Logs.debug (fun m -> m "✓ ethernet plugged (%a)" Macaddr.pp (Ethernet.mac eth));
      let* arpv4_daemon, arpv4 = ARPv4.create ~ipaddr:(Ipaddr.V4.Prefix.address cidr) eth in
      Logs.debug (fun m -> m "✓ ARPv4 daemon launched");
      let* ipv4 = IPv4.create eth arpv4 ?gateway cidr in
      Logs.debug (fun m -> m "✓ IPv4 stack created");
      let icmpv4 = ICMPv4.handler ipv4 in
      Logs.debug (fun m -> m "✓ ICMPv4 daemon launched");
      let tcpv4_daemon, tcpv4 = TCPv4.create ~name:"uniker.ml" ipv4 in
      let udpv4 = TCPv4.udpv4 tcpv4 in
      Logs.debug (fun m -> m "✓ TCPv4 daemon launched");
      IPv4.set_handler ipv4 (ipv4_handler icmpv4 udpv4 tcpv4);
      let fn = ethernet_handler arpv4 ipv4 in
      Ethernet.set_handler eth fn;
      Ok ({ ethernet_daemon= daemon; arpv4_daemon; udpv4; icmpv4; tcpv4_daemon }, tcpv4) in
    let mac = Macaddr.of_octets_exn (cfg.Miou_solo5.Net.mac :> string) in
    match connect mac with
    | Ok daemon -> daemon
    | Error err -> Fmt.failwith "%a" pp_error err in
  Miou_solo5.(map fn [ net name ])
