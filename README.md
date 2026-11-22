## µTCP - Transmission control protocol

This repository contains a TCP stack with a pure functional core. Its design is
close to the [HOL4 specification](https://www.cl.cam.ac.uk/~pes20/Netsem/alldoc.pdf)
developed in the [Netsem](https://www.cl.cam.ac.uk/~pes20/Netsem/) project -
have a look at the
[overview paper](http://www.cl.cam.ac.uk/~pes20/Netsem/paper3.pdf) if interested.
The [GitHub repository](https://github.com/rems-project/netsem) is the current
development head - and the code in here uses the FreeBSD-CURRENT branch.
If you find function names that are confusing, its worth to look into the NetSem
specification, there may exist a rule with the same name with same behaviour.

In contrast to the basic TCP [RFC 793](https://tools.ietf.org/html/rfc793), some
features are excluded:
- Out of band data (urgency, urgent pointers)
- No backlog queue, lots of connections have to be throttled elsewhere
- Timestamps

Very informative to read is [RFC 7414](https://tools.ietf.org/html/rfc7414) - a
roadmap for TCP specification documents, and
[RFC 9293](https://tools.ietf.org/html/rfc9293) TCP
specification (which combines and obsoletes several RFCs).

Initially, I thought timestamp option would be a good idea, but then they mainly
are privacy risks exposing a global counter (you could do a per-connection
timestamp), and add _12_ bytes to _every_ segment for basically no benefit (see
https://www.snellman.net/blog/archive/2017-07-20-s3-mystery/ for some discussion
thereof).

In contrast to above specifications, the state transition diagram is different
in this implementation. The reason is that not the full Unix sockets API is
implemented, especially listening sockets are treat specially instead of being
fused into the state machine: each (passive) connection starts in the
SYN_RECEIVED state, there is no LISTEN state. There is also no CLOSED state -
a connection which would end up in this state is directly dropped.

Features included in this implementation that have been specified in later RFCs:
- [RFC 1337](https://tools.ietf.org/html/rfc1337) - TIME-WAIT Assassination hazards
- [RFC 4987](https://tools.ietf.org/html/rfc4987) - SYN-flood attacks
- [RFC 5927](https://tools.ietf.org/html/rfc5927) - ICMP Attacks
- [RFC 5961](https://tools.ietf.org/html/rfc5961) - blind in-window attacks (+ FreeBSD extension for zero window + delayed ACKs)
- [RFC 6528](https://tools.ietf.org/html/rfc6528) - defending against sequence number attacks
- [RFC 7323](https://tools.ietf.org/html/rfc7323) - TCP Extensions for high-performance
- [draft-gont-tcpm-tcp-seq-validation-04](https://datatracker.ietf.org/doc/html/draft-gont-tcpm-tcp-seq-validation-04) - On the Validation of TCP Sequence Numbers

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

- Urgent flag (not handled in the implementation)
- More restrictive with flag combinations (only one of SYN FIN RST)
- `CLOSED` state can't be observed
- going from `TIME_WAIT` anywhere (i.e. when someone connects with a socket, and instead close on EOF does another connect - this may actually happen; if you're talking to this library, your second connect will fail.... hope you handle the case properly) - `deliver_in_9` will never happen for us (see above, RFC 1337)
- `TCP_NEWRENO` is true (we skip the conditionals)
- We have infinite resources (well, of course not, but: on the send edge the buffer is provided by the caller (and then owned by us); on the receive side the buffer is provided by the caller as well) -> we don't really allocate data (apart from some records/..), but we nevertheless limit `rcvbufsize` to 2^16 (user-configurable via `Utcp.empty ~sndbuf ~rcvbuf ..`)
- There is no bandwidth limitation, output always succeeds (this simplifies a lot)! no rollback and `enqueue_or_fail`

Model anomalies:
- is tcp option size computation good in timer_tt_rexmtsyn_1? (missing MSS)
- tt_persist doesn't check whether shift + 1 is < tcp_maxrxtshift, and also there's never a drop (after the tcp_backoff array has been used up) -- there is a case in FreeBSD (tcp_timer.c): "tp->t_rxtshift >= V_tcp_retries && (ticks - tp->t_rcvtime >= tcp_maxpersistidle || ticks - tp->t_rcvtime >= TCP_REXMTVAL(tp) * tcp_totbackoff))" -- while our implementation only implements the latter at the moment
- di3_ackstuff: hostLTS:452 "ack <= snd_una", but text "strictly less than snd_una"
- di3_newackstuff: hostLTS:251 uses "cb'.snd_nxt" which is the same (and a no-op)
- we update rcv_wnd (of the control block) in tcp_output_really, the model does not (see TCP1_hostTypesScript.sml:538 - they compute the rcv_wnd lazily)
- the timer_tt_rexmt_1 is restricted to tcp_state <> [ CLOSED; LISTEN; ] SYN SENT; CLOSE WAIT; FIN WAIT 2; TIME WAIT, but in CLOSE WAIT we need a retransmit timer, implementation changed in f3a083fc082f580e387917ebbb135b1c940fd5bc
- no tf_shouldacknow in close or shutdown (if the write part is shutdown, the cantsndmore signals tcp_output_* that a fin is needed anyways)
- tcp_output_really check for snd_nxt should include una = iss & nxt - iss = 2 -- the reasoning is that otherwise a TCP connection where SYN has been acked, and a single byte is transferred and then FIN is sent ends in the same case - see 119b0362d187cf40da149b56d911e840864c201a (FreeBSD has TF_SENTFIN for this)
- when rcv_nxt > rcv_adv, move window to 0 - 79dae8ee196f2c7072a12c6d3034005a8a6c6dca (same as FreeBSD)

When to output segments?
- basically on every incoming segment, on any user operation, and any timer (mostly rexmt, but as well persist and delack)
- in tcp_output we establish a list of segments to be sent (if any), avoiding the necessity to wait for other interaction if the congestion window is big enough

## Installation

`opam install utcp`

## License

It is licensed under the AGPL-3.0 or later. If you want to pay for a more
permissive license, reach out to team@robur.coop.

## AI

Contributions involving AI must comply with the rules set out [here](./AI.md).
If they do not comply, we reserve the right to reject your contribution.

## Funding

This project is funded through [NGI0 Commons Fund](https://nlnet.nl/commonsfund), a fund established by [NLnet](https://nlnet.nl) with financial support from the European Commission's [Next Generation Internet](https://ngi.eu) program. Learn more at the [NLnet project page](https://nlnet.nl/project/microTCP). The amount is 50000 EUR.
