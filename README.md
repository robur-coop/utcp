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

Very informative to read is [RFC 7414](https://tools.ietf.org/html/rfc7414) - a
roadmap for TCP specification documents, and
[RFC 793bis](https://tools.ietf.org/html/draft-ietf-tcpm-rfc793bis-13) TCP
specification (which combines several RFCs).

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
- push handling / when to signal "received data"
- when to send data (793bis has some information about it)
- path MTU discovery (RFC 1191, including array - also 793bis)
- ICMP error handling!
- ABC (not in the HOL4 model, though :/)
- increased initial window size
- timestamping
- PAWS
- SACK

- CC
