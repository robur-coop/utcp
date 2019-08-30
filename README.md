## TCP - Transmission control protocol

This repository contains a TCP stack with a pure functional core. Its design is
close to the [HOL4 specification](https://www.cl.cam.ac.uk/~pes20/Netsem/alldoc.pdf)
developed in the [Netsem](https://www.cl.cam.ac.uk/~pes20/Netsem/) project -
have a look at the
[overview paper](http://www.cl.cam.ac.uk/~pes20/Netsem/paper3.pdf) if interested.
If you find function names that are confusing, its worth to look into the NetSem
specification, there may exist a rule with the same name with same behaviour.

In contrast to the basic TCP [RFC 793](https://tools.ietf.org/html/rfc793), some
features are excluded:
- Out of band data (urgency, urgent pointers)
- No backlog queue, lots of connections have to be throttled elsewhere
- Timestamps

Very informative to read is [RFC 7414](https://tools.ietf.org/html/rfc7414) - a
roadmap for TCP specification documents, and
[RFC 793bis](https://tools.ietf.org/html/draft-ietf-tcpm-rfc793bis-13) TCP
specification (which combines several RFCs).

Initially, I thought timestamp option would be a good idea, but then they mainly
are privacy risks exposing a global counter (you could do a per-connection
timestamp), and add _12_ bytes to _every_ segment for basically no benefit (see
https://www.snellman.net/blog/archive/2017-07-20-s3-mystery/ for some discussion
thereof).

In contrast to above specifications, the state transition diagram is different
in this implementation. The reason is that not the full Unix sockets API is
implemented, especially listening sockets are treat specially instead of being
fused into the state machine (each (passive) connection starts in the
SYN_RECEIVED state, there is no LISTEN state). There is also no CLOSED state -
a connection which would end up in this state is directly dropped.

TODO: Features included in this implementation that have been specified in later RFCs:
- [RFC 1337](https://tools.ietf.org/html/rfc1337) - TIME-WAIT Assassination hazards
- [RFC 4987](https://tools.ietf.org/html/rfc4987) - SYN-flood attacks
- [RFC 5927](https://tools.ietf.org/html/rfc5927) - ICMP Attacks
- [RFC 5961](https://tools.ietf.org/html/rfc5961) - blind in-window attacks
- [RFC 6528](https://tools.ietf.org/html/rfc6528) - defending against sequence number attacks
- [RFC 7323](https://tools.ietf.org/html/rfc7323) - TCP Extensions for high-performance

TODO:
- push handling / when to signal "received data" (nagle)
- when to send data (793bis has some information about it)
- path MTU discovery (RFC 1191, including array - also 793bis)
- ICMP error handling!
- appropriate byte counting (RFC 3456, not in the HOL4 model, though :/)
- increased initial window size
- SACK
- CC

## Intentional differences to the NetSem specification

The specification covers all valid executions of what is known as TCP. This is
an implementation taking choices (sometimes early) and not implementing the full
specification - sometimes even violating it on purpose!

A plan is to use the specification and the test harness (+packetdrill and the
FreeBSD TCP testsuite) to validate that the implementation behaves according to
the specification. Since the specification is at the moment specialised on
FreeBSD-12, this will likely need some work. In this place, I attempt to document
which bits and pieces need to be touched. For reference, all comments and line
numbers are refering to git commit 2374ad26b2f4f32f62aaea62ac641c3a91b2efbc
(mostly actually 409966517e3468bc677d58f46756957d4a1dddb0, because the other is
rather private).

We'll likely need to define a new ARCH for this implementation in the model and
then hope it's good enough ;) (or only run tests which behave similat to
FreeBSD).

- Appropriate Byte Counting (this is what i intend to implement)
- Initial window size (that may already be in my FreeBSD12 changeset)
- Incoming urgent flag (not handled in the implementation)
- More restrictive with flag combinations (only one of SYN FIN RST)
- CLOSED state can't be observed
- going from TIME_WAIT anywhere (i.e. when someone connects with a socket, and instead close on EOF does another connect - this may actually happen; if you're talking to this library, your second connect will fail.... hope you handle the case properly) - deliver_in_9 will never happen for us
- TCP_NEWRENO is true (we skip the conditionals)
- We have infinite resources (well, of course not, but: on the send edge the buffer is provided by the caller (and then owned by us); on the receive side the buffer is provided by the caller as well)
  -> we don't really allocate data (apart from some records/..), but we nevertheless limit rcvbufsize to 2^16 (should be user-configurable)
- There is no bandwidth limitation, output always succeeds (this simplifies a lot)!
  -> no rollback / enqueue_or_fail

Model anomalies:
- is tcp option size computation good in timer_tt_rexmtsyn_1? (missing MSS)
- tt_persist doesn't check whether shift + 1 is < tcp_maxrxtshift
- di3_ackstuff: hostLTS:452 "ack <= snd_una", but text "strictly less than snd_una"
- di3_newackstuff: hostLTS:251 uses "cb'.snd_nxt" which is the same (and a no-op)

## Things to preserve and ensure

- each incoming segment with reasonable window is handled properly
- there's always a path (e.g. via timers) to drop the connection (with/out RST)
  - that'll be hard
- timers: is one sufficient (with either rexmtsyn, persist, idle, rexmt)?
  - no, at least time_wait and fin_wait_2 are special afaict
  - see also comment in hostTypes:301
- esp in the connection setup states (syn_sent/syn_received) and connection
  teardown states (close_wait/fin_wait_1/..) ensure that the pcb will eventually
  be discarded
- what about outgoing segments? should Input.handle be able to return
  (a) an optional Segment.t (as in deliver_in_* [apart from 3])
  (b) a list of Segment.t
  --> b sounds more plausible - and ACK may open snd_wnd so that multiple data
      segments are transmitted!

## TODO

- suspicious that someone/somehow deliver_out_1 needs to be triggered... but at
  which points in time? at the end of <got a segment from network and didn't
  provoke a reply>? <got some data from user>?
  i think delack timer is properly initialised when needed (but getting a FIN
  leads to no reply (though tf_shouldacknow is true now))
  -- maybe i'm just missing a tcp_output(_perhaps?) call

- just copied over various functions which need to be properly tested:
 - RTT measurement
 - maximum segment size computation
 - all duration and timer computations...
- should data = [] be more explicitly assert in early handshake?
  - from what I understand
    - SYN may not carry data (apart from TFO where server may send data in SYN+ACK)
    - RST may carry data (that's the "error message"), FreeBSD may (used to?) send random sndq data

- FLAGS rework
  - ack : Sequence.t option ; push : bool ; control : [ `SYN | `FIN | `RST ]
  --> we then need to error with "has reset" and "has_ack" and "seq of Sequence.t" (to properly reply)
  --> or just drop such segments without notifying the other side..
  from draft-rfc793bis:
    CLOSED: ACK set/unset
    LISTEN: first RST (ignore), second ACK (RST), third SYN, fourth others (ignore RST, drop)
    SYN-SENT: first ACK, second RST, SYN
    others: check sequence number, then RST, then SYN, then ACK, then FIN
- provide a Sequence.Infix module with < <= > >= = + ++ (where + is weird: Sequence.t -> int -> Sequence.t)
- error handling (temporary errors / error types to present)

- error propagation: cb can get some errors (from ip / icmp)
   (maybe temporary) which are preserved in softerror, and bubble up
   [also timeouts] <- this is to-be-returned when connect/read/write/close fails
- icmp also for path-mtu
- when is t_maxseg set? is it modified at all? (should not be once ESTABLISHED is reached)

- t_badrxtwin <- meh (don't understand its value and usage)

- really need to ensure that we're not talking to ourselves in Segment.decode_and_verify...

- what about multiple local ip addresses? (boils down to how to select IP in connect_1 [and avoid all t.ip...] well, and dropwithreset that crafts a RST of a segment)
  (we could embed peer IP into control-block / socket)

- segment reassembly
- put cc in a separate module, follow FreeBSD design ack_received / after_idle / conf_signal / post_recovery
- tcp_output_really and tcp_do_output have quite some code shared...
- keepalive is in the model, could easily be copied over

- recheck with draft793bis whether (all of) our transitions are legitimate and none is really missing

- further avoiding allocations: sndq and rcvq should be lists of cstruct (no need for Cstruct.append!)

- make the bsd_fast_path a fast path for us ;)
- the rcv_window computations are done for bad segments (di_2a/7c/7d) on BSD as well, but we don't need that behaviour

## Testing

- packetdrill-like scripts!?
- luckily with a pure API we can test this directly (no need for sockets, and
  actual wire transmission)
- downside is we need to develop/adapt a syntax for packet building (and
  expected answers within time boundaries), and write the test cases
  (but then I'm not really able to find well-engineered tests with packetdrill
   - yes, the FreeBSD suite is nice, but contains quite some copy + paste)
   also, packetdrill is GPL (but lots of tests BSD3)
- maybe we can have both -- first our own tests, at a later point write a
  packetdrill remote helper that translates commands into OCaml calls and
  this way execute and evaluate those tests
- https://github.com/freebsd-net/tcp-testsuite/tree/master/state-event-engine
- there's also tthee (part of netsem), extensive ad-hoc tests (with remote
  helpers) of unix sockets API - the traces have been evaluated with FreeBSD
  4.6 to some degree!

### Test notes

a matrix from testing LISTEN and CLOSED ports, tested with FreeBSD and Linux:

                 LISTEN               CLOSED
            FreeBSD   Linux      FreeBSD     Linux
NONE           -        -          RST+ACK(2) RST+ACK(2)
FIN            -        -          RST+ACK(2) RST+ACK(2)
FIN+ACK(+data)  RST     RST        RST        RST
ACK(+data)      RST     RST        RST        RST
RST(+ACK)(+SYN)(+FIN)-  -           -          -
SYN          SYN+ACK    SYN+ACK    RST+ACK(2) RST+ACK(2)
SYN+ACK         RST     RST        RST        RST
SYN+FIN(+data)SYN+ACK(1)  -        RST+ACK(2) RST+ACK(2)
SYN+data     SYN+ACK(1) SYN+ACK(1) RST+ACK(2) RST+ACK(2)

1: only the SYN is acked, not FIN or data!
2: ACK includes data and fin

## Further notes

- from rationale.txt:208 cantrcvmore: this is equivalent to ``st IN {CLOSE_WAIT,
  LAST_ACK, CLOSING; TIME_WAIT; CLOSED}``.  And FIN_WAIT_1???
  invariants.txt:101
  If cantrcvmore is set, then rcvq never grows.
  Hypothesis:
  cantrcvmore <== st IN { CLOSE_WAIT; LAST_ACK; FIN_WAIT_1;
                          FIN_WAIT_2; CLOSING; TIME_WAIT }
  ~cantrcvmore <== st IN { ESTABLISHED; SYN_SENT; SYN_RCVD }
  think we don't care in LISTEN or CLOSED.
  ==> if we believe this, we should remove cantrcvmore.
  (note that cantsndmore is different; it merely records that we intend
  to send a FIN (and change state) at some point in the future (not
  necessarily now).)

- a "read shutdown" leads in FreeBSD to drop any incoming data (but still
  processing segments AFAICT), imagine we set window size to 0 (so the remote
  won't ever bother sending us more data) <- will reduce required bandwidth
  on the flip side: the remote stack will start a timer to figure out when the
  window opens again, since it can't know that window==0 will stay forever
  ~> need to research the actual behaviour once the write part is finished as
     well and we send a fin (the other side may still try to send us their sndq,
     until some timeout)
  ~~> for app-level protocols (i.e. TLS) for proper teardown they want to send
      some data (alert close_notify), which will be avoided
  ~> may negatively impact the other tcp state (and keep us in fin_wait_2 for
     a long time (with the timer being reset by window probes))
  ~> likely depends on the LINGER option set on the other side, whether to wait
     in close() until delivered

- RCVTIMEO and SNDTIMEO <- what exactly do they time? until accepted in
  sndq/rcvq?
