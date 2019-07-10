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
- ABC (not in the HOL4 model, though :/)
- increased initial window size
- PAWS (well, maybe... really?)
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

- Accurate Byte Counting (this is what i intend to implement)
- Initial window size (that may already be in my FreeBSD12 changeset)
- Incoming urgent flag (not handled in the implementation)
- More restrictive with flag combinations (uhm, likely... but then, half-closed connection are fun, maybe FIN+SYN is a valid combination ;)
- CLOSED state can't be observed
- going from TIME_WAIT anywhere (i.e. when someone connects with a socket, and instead close on EOF does another connect - this may actually happen; if you're talking to this library, your second connect will fail.... hope you handle the case properly) - deliver_in_9 will never happen for us

## Questions

Is a reset ever retransmitted? no!
