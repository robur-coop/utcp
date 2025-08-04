open Utcp

let simple =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  let r = Rope.empty in
  let c = Cstruct.of_string "Hello World!" in
  let r = Rope.prepend (Cstruct.sub c 0 5) r in
  let r = Rope.append ~off:5 ~len:1 r c in
  let r = Rope.append ~off:6 r c in
  Alcotest.(check string) "to_string" (Rope.to_string r) "Hello World!";
  Alcotest.(check string) "sub" Rope.(to_string (sub ~off:0 ~len:5 r)) "Hello"

let cstruct = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal

let never_empty =
  Alcotest.test_case "no empty slices" `Quick @@ fun () ->
  let r = Rope.empty in
  Alcotest.(check (list cstruct)) "empty" (Rope.to_css r) [];
  let r = Rope.prepend Cstruct.empty r in
  Alcotest.(check (list cstruct)) "empty" (Rope.to_css r) [];
  let r = Rope.append r Cstruct.empty in
  Alcotest.(check (list cstruct)) "empty" (Rope.to_css r) [];
  let r = Rope.of_css [ Cstruct.empty; Cstruct.empty ] in
  Alcotest.(check (list cstruct)) "empty" (Rope.to_css r) [];
  let r = Rope.of_cs Cstruct.empty in
  Alcotest.(check (list cstruct)) "empty" (Rope.to_css r) [];
  let foo = Cstruct.of_string "foo" in
  let bar = Cstruct.of_string "bar" in
  let r = Rope.of_css [ foo; Cstruct.empty; bar ] in
  Alcotest.(check (list cstruct)) "foo/bar" (Rope.to_css r) [ foo; bar ];
  let r = Rope.prepend Cstruct.empty r in
  let r = Rope.append r Cstruct.empty in
  Alcotest.(check (list cstruct)) "foo/bar" (Rope.to_css r) [ foo; bar ]

let length =
  Alcotest.test_case "length" `Quick @@ fun () ->
  let r = Rope.empty in
  Alcotest.(check int) "length" (Rope.length r) 0;
  let r = Rope.of_css [ Cstruct.empty; Cstruct.empty ] in
  Alcotest.(check int) "length" (Rope.length r) 0;
  let r = Rope.of_string "Hello" in
  Alcotest.(check int) "hello" (Rope.length r) 5;
  let r = Rope.prepend Cstruct.empty r in
  let r = Rope.append r Cstruct.empty in
  Alcotest.(check int) "hello" (Rope.length r) 5;
  let foo = Cstruct.of_string "foo" in
  let bar = Cstruct.of_string "bar" in
  let r = Rope.of_css [ foo; Cstruct.empty; Cstruct.empty; bar ] in
  Alcotest.(check int) "foo/bar" (Rope.length r) 6

let tests = [ simple; never_empty; length ]
