(* (c) 2019 Hannes Mehnert, all rights reserved *)

let mclbytes = 2048
and msize = 256
and sb_max = 256 * 1024

(* params:450 *)
let so_sndbuf = 32 * 1024
and so_rcvbuf = 65535
and so_sndlowat = 2048
and so_rcvlowat = 1
and so_min_sndbuf = 1
and so_min_rcvbuf = 1
and so_min_sndlowat =1
and so_min_rcvlowat = 1
and so_max_sndbuf = sb_max * mclbytes / (mclbytes + msize)
and so_max_rcvbuf = sb_max * mclbytes / (mclbytes + msize)
and so_max_sndlowat = sb_max * mclbytes / (mclbytes + msize)
and so_max_rcvlowat = sb_max * mclbytes / (mclbytes + msize)

and dtsinval = Duration.of_day 24

and tcp_maxwin = 65535
and tcp_maxwinscale = 14
and mssdflt = 536
and ss_fltsz = 1
and scale = 6

(* updated from FreeBSD 13 *)
and tcptv_msl = Duration.of_sec 30
and tcptv_srttbase = 0L
and tcptv_rtobase = Duration.of_sec 3
and tcptv_persmin = Duration.of_sec 5
and tcptv_persmax = Duration.of_sec 60
and tcptv_keep_init = Duration.of_sec 75
and tcptv_keep_idle = Duration.of_hour 2
and tcptv_keepintvl = Duration.of_sec 75
and tcptv_keepvnt = 8
and tcptv_finwait2_timeout = Duration.of_sec 60

let tcptv_maxidle = Int64.shift_left tcptv_keepintvl 3

and tcptv_min = Duration.of_ms 30
and tcptv_cpu_var = Duration.of_ms 200
and tcptv_rexmtmax = Duration.of_sec 64
and tcptv_twtrunc = 8
and tcp_lingertime = Duration.of_min 2
and tcp_maxrxtshift = 12
and tcp_synackmaxrxtshift = 3
and tcptv_delack = Duration.of_ms 100
and tcptv_rttvarbase = 0L

let tcp_rtt_invalidate = tcp_maxrxtshift / 4
and tcp_syn_backoff =
    [| 1L ; 1L ; 1L ; 1L ; 1L ; 2L ; 4L ; 8L ; 16L ; 32L ; 64L ; 64L ; 64L |]
and tcp_backoff =
    [| 1L ; 2L ; 4L ; 8L ; 16L ; 32L ; 64L ; 128L ; 256L ; 512L ; 512L ; 512L ; 512L |]
