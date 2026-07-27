open Utcp

let simple =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  let r = Rope.empty in
  let c = "Hello World!" in
  let r = Rope.prepend (String.sub c 0 5) r in
  let r = Rope.append ~off:5 ~len:1 r c in
  let r = Rope.append ~off:6 r c in
  Alcotest.(check string) "to_string" "Hello World!" (Rope.to_string r);
  Alcotest.(check string) "chop" "Hello" Rope.(to_string (chop r 5))

let never_empty =
  Alcotest.test_case "no empty slices" `Quick @@ fun () ->
  let r = Rope.empty in
  Alcotest.(check (list string)) "empty" [] (Rope.to_strings r);
  let r = Rope.prepend String.empty r in
  Alcotest.(check (list string)) "empty" [] (Rope.to_strings r);
  let r = Rope.append r String.empty in
  Alcotest.(check (list string)) "empty" [] (Rope.to_strings r);
  let r = Rope.of_strings [ String.empty; String.empty ] in
  Alcotest.(check (list string)) "empty" [] (Rope.to_strings r);
  let r = Rope.of_string String.empty in
  Alcotest.(check (list string)) "empty" [] (Rope.to_strings r);
  let foo = "foo" in
  let bar = "bar" in
  let r = Rope.of_strings [ foo; String.empty; bar ] in
  Alcotest.(check string) "foo/bar" (foo ^ bar) (Rope.to_string r);
  let r = Rope.prepend String.empty r in
  let r = Rope.append r String.empty in
  Alcotest.(check string) "foo/bar" (foo ^ bar) (Rope.to_string r)

let length =
  Alcotest.test_case "length" `Quick @@ fun () ->
  let r = Rope.empty in
  Alcotest.(check int) "length" 0 (Rope.length r);
  let r = Rope.of_strings [ String.empty; String.empty ] in
  Alcotest.(check int) "length" 0 (Rope.length r);
  let r = Rope.of_string "Hello" in
  Alcotest.(check int) "hello" 5 (Rope.length r);
  let r = Rope.prepend String.empty r in
  let r = Rope.append r String.empty in
  Alcotest.(check int) "hello" 5 (Rope.length r);
  let foo = "foo" in
  let bar = "bar" in
  let r = Rope.of_strings [ foo; String.empty; String.empty; bar ] in
  Alcotest.(check int) "foo/bar" 6 (Rope.length r)

let tests = [ simple; never_empty; length ]
