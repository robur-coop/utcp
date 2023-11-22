open Utcp

let src = Ipaddr.(V4 (V4.of_string_exn "10.0.0.1"))
and dst = Ipaddr.(V4 (V4.of_string_exn "10.0.0.2"))

let simple () =
  let t =
    Segment.{ src_port = 10 ; dst_port = 100 ; seq = Sequence.zero ;
              ack = None ; flag = None ; push = false ; window = 10 ;
              options = [] ; payload = Cstruct.empty }
  in
  Alcotest.(check int "simple" 39786 Segment.(checksum ~src ~dst (encode t)))

let data1 () =
  let payload = Cstruct.of_hex "01" in
  let t =
    Segment.{ src_port = 10 ; dst_port = 100 ; seq = Sequence.zero ;
              ack = None ; flag = None ; push = false ; window = 10 ;
              options = [] ; payload }
  in
  Alcotest.(check int "data1" 39529 Segment.(checksum ~src ~dst (encode t)))

let data2 () =
  let payload = Cstruct.of_hex "01 02" in
  let t =
    Segment.{ src_port = 10 ; dst_port = 100 ; seq = Sequence.zero ;
              ack = None ; flag = None ; push = false ; window = 10 ;
              options = [] ; payload }
  in
  Alcotest.(check int "data2" 39526 Segment.(checksum ~src ~dst (encode t)))

let data3 () =
  let payload = Cstruct.of_hex "01 02 03" in
  let t =
    Segment.{ src_port = 10 ; dst_port = 100 ; seq = Sequence.zero ;
              ack = None ; flag = None ; push = false ; window = 10 ;
              options = [] ; payload }
  in
  Alcotest.(check int "data3" 38757 Segment.(checksum ~src ~dst (encode t)))

let data4 () =
  let payload = Cstruct.of_hex "01 02 03 04" in
  let t =
    Segment.{ src_port = 10 ; dst_port = 100 ; seq = Sequence.zero ;
              ack = None ; flag = None ; push = false ; window = 10 ;
              options = [] ; payload }
  in
  Alcotest.(check int "data4" 38752 Segment.(checksum ~src ~dst (encode t)))

let data5 () =
  let payload = Cstruct.of_hex "01 02 03 04 05" in
  let t =
    Segment.{ src_port = 10 ; dst_port = 100 ; seq = Sequence.zero ;
              ack = None ; flag = None ; push = false ; window = 10 ;
              options = [] ; payload }
  in
  Alcotest.(check int "data5" 37471 Segment.(checksum ~src ~dst (encode t)))

let real1 () =
  let data = Cstruct.of_hex {|
00 50 d0 c5 90 3b 6f b9 31 8e 90 da 70 12 ff ff
83 98 00 00 02 04 05 b4 03 03 06 00|}
  in
  let src = Ipaddr.(V4 (V4.of_string_exn "10.0.42.2"))
  and dst = Ipaddr.(V4 (V4.of_string_exn "10.0.42.1"))
  in
  Alcotest.(check int "real1" 0x8399 Segment.(checksum ~src ~dst data))

let tests = [
  "simple", `Quick, simple ;
  "data1", `Quick, data1 ;
  "data2", `Quick, data2 ;
  "data3", `Quick, data3 ;
  "data4", `Quick, data4 ;
  "data5", `Quick, data5 ;
  "real1", `Quick, real1 ;
]
