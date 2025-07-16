
open Utcp
open State.Reassembly_queue

let empty_is_empty () =
  Alcotest.(check int "empty reassembly is empty" 0 (length empty))

let added_is_nonempty () =
  let r = insert_seg empty (Sequence.zero, false, String.empty) in
  Alcotest.(check int "reassembly queue is not empty" 1 (length r))

let data = String.make 10 '\042'

let added_can_be_taken () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, fin) ->
    Alcotest.(check bool "data should be fine" true (String.equal data s));
    Alcotest.(check bool "fin should be false" false fin)

let added_can_be_taken2 () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  let r, s = maybe_take r (Sequence.of_int32 5l) in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> ()
  | Some (s, fin) ->
    Alcotest.(check int "data should be fine" 5 (String.length s));
    let exp_data = String.sub data 5 (String.length data - 5) in
    Alcotest.(check bool "data should be fine" true (String.equal s exp_data));
    Alcotest.(check bool "fin should be false" false fin)

let added_can_be_taken3 () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  let r, s = maybe_take r (Sequence.of_int32 10l) in
  Alcotest.(check int "reassembly queue dropped all segments" 0 (length r));
  match s with
  | None -> ()
  | Some _ -> Alcotest.fail "there shouldn't be anything left"

let coalescing_works () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  let r = insert_seg r (Sequence.of_int32 10l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) ->
    Alcotest.(check bool "data is good" true
                (String.equal s (String.concat "" [data; data ])))

let coalescing_works_rev () =
  let r = insert_seg empty (Sequence.of_int32 10l, false, data) in
  let r = insert_seg r (Sequence.zero, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) ->
    Alcotest.(check bool "data is good" true
                (String.equal s (String.concat "" [data; data ])))

let coalescing_works_3 () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 30l, false, data) in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  let r = insert_seg r (Sequence.of_int32 20l, false, data) in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  let r = insert_seg r (Sequence.of_int32 10l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) ->
    Alcotest.(check bool "data is good" true
                (String.equal s (String.concat "" [ data ; data ; data ; data ])))

let coalescing_works_4 () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 15l, false, data) in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  let r = insert_seg r (Sequence.of_int32 10l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 20l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 30 (String.length s))

let coalescing_works_5 () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 1l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 2l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 3l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 4l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 5l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 6l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 7l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 8l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 9l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 10l, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 20 (String.length s))

let coalescing_works_6 () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 50l, false, data) in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  let r = insert_seg r (Sequence.of_int32 30l, false, data) in
  Alcotest.(check int "reassembly queue is now three elements" 3 (length r));
  let r = insert_seg r (Sequence.of_int32 20l, false, data) in
  Alcotest.(check int "reassembly queue is now three elements" 3 (length r));
  let r = insert_seg r (Sequence.of_int32 50l, false, data) in
  Alcotest.(check int "reassembly queue is now three elements" 3 (length r));
  let r = insert_seg r (Sequence.of_int32 11l, false, data) in
  Alcotest.(check int "reassembly queue is now three elements" 3 (length r));
  let r = insert_seg r (Sequence.of_int32 1l, false, data) in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 40 (String.length s))

let coalescing_works_7 () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  let r = insert_seg r (Sequence.of_int32 50l, false, data) in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  let r = insert_seg r (Sequence.of_int32 40l, false, data) in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  let r = insert_seg r (Sequence.of_int32 20l, false, data) in
  Alcotest.(check int "reassembly queue is now three elements" 3 (length r));
  let r = insert_seg r (Sequence.of_int32 45l, false, data) in
  Alcotest.(check int "reassembly queue is now three elements" 3 (length r));
  let r = insert_seg r (Sequence.of_int32 15l, false, data) in
  Alcotest.(check int "reassembly queue is now three elements" 3 (length r));
  let r, s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now two elements" 2 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 10 (String.length s));
  let r, s = maybe_take r (Sequence.of_int32 15l) in
  Alcotest.(check int "reassembly queue is now one element" 1 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 15 (String.length s));
  let r, s = maybe_take r (Sequence.of_int32 45l) in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 15 (String.length s))

let take_works () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  let r = insert_seg r (Sequence.of_int32 10l, false, data) in
  let r = insert_seg r (Sequence.of_int32 20l, false, data) in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 30 (String.length s));
  let r', s = maybe_take r (Sequence.of_int32 15l) in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check int "data is good" 15 (String.length s));
  let r', s = maybe_take r (Sequence.of_int32 45l) in
  Alcotest.(check int "reassembly queue is now empty (has been pruned)" 0 (length r'));
  match s with
  | None -> ()
  | Some _ -> Alcotest.fail "there shouldn't be anything"

let take_works_taking_before () =
  let r = insert_seg empty (Sequence.zero, false, data) in
  let r', s = maybe_take r (Sequence.of_int32 20l) in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> ()
  | Some _ -> Alcotest.fail "there shouldn't be anything"

let take_works_taking_before_2 () =
  let r = insert_seg empty (Sequence.of_int32 20l, false, data) in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is not empty" 1 (length r'));
  match s with
  | None -> ()
  | Some _ -> Alcotest.fail "there shouldn't be anything"

let overlap_1 () =
  let r = insert_seg empty (Sequence.of_int32 16l, false, "AAAAAAAA") in
  let r = insert_seg r (Sequence.of_int32 8l, false, "BBBBBBBBBB") in
  let r = insert_seg r (Sequence.of_int32 0l, false, "CCCCCCCCCC") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "CCCCCCCCCCBBBBBBBBAAAAAA" s)

let overlap_2 () =
  let r = insert_seg empty (Sequence.of_int32 8l, false, "BBBBBBBBBB") in
  let r = insert_seg r (Sequence.of_int32 16l, false, "AAAAAAAA") in
  let r = insert_seg r (Sequence.of_int32 0l, false, "CCCCCCCCCC") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "CCCCCCCCCCBBBBBBAAAAAAAA" s)

let overlap_3 () =
  let r = insert_seg empty (Sequence.of_int32 16l, false, "AAAAAAAA") in
  let r = insert_seg r (Sequence.of_int32 8l, false, "BBBBBBBB") in
  let r = insert_seg r (Sequence.of_int32 0l, false, "CCCCCCCCCC") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "CCCCCCCCCCBBBBBBAAAAAAAA" s)

let overlap_4 () =
  let r = insert_seg empty (Sequence.of_int32 16l, false, "AAAAAAAA") in
  let r = insert_seg r (Sequence.of_int32 8l, false, "BBBBBB") in
  let r = insert_seg r (Sequence.of_int32 6l, false, "CCCCCCCCCCCCCCCC") in
  let r = insert_seg r (Sequence.of_int32 0l, false, "DDDDDDDD") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "DDDDDDDDCCCCCCCCCCCCCCAA" s)

let overlap_5 () =
  let r = insert_seg empty (Sequence.of_int32 0l, false, "AAAAAAAAAAAAAAAA") in
  let r = insert_seg r (Sequence.of_int32 4l, false, "BBBBBB") in
  let r = insert_seg r (Sequence.of_int32 10l, false, "CCCCCC") in
  let r = insert_seg r (Sequence.of_int32 12l, false, "DDDDDD") in
  let r = insert_seg r (Sequence.of_int32 14l, false, "EE") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "AAAABBBBBBCCDDEEDD" s)

let overlap_6 () =
  let r = insert_seg empty (Sequence.of_int32 0l, false, "AAAAAAAA") in
  let r = insert_seg r (Sequence.of_int32 10l, false, "BBBBBB") in
  let r = insert_seg r (Sequence.of_int32 8l, false, "DDDD") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "AAAAAAAADDDDBBBB" s)

let regr_187 () =
  let r = insert_seg empty (Sequence.of_int32 16l, false, "000002om") in
  let r = insert_seg r (Sequence.of_int32 8l, false, "001001nn001002nm001003nl") in
  let r = insert_seg r (Sequence.of_int32 0l, false, "002000mo") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "002000mo001001nn001002nm001003nl" s)

let regr_188 () =
  let r = insert_seg empty (Sequence.of_int32 24l, false, "000003ol") in
  let r = insert_seg r (Sequence.of_int32 8l, false, "001001nn001002nm001003nl001004nk") in
  let r = insert_seg r (Sequence.of_int32 0l, false, "002000mo002001mn") in
  let r', s = maybe_take r Sequence.zero in
  Alcotest.(check int "reassembly queue is now empty" 0 (length r'));
  match s with
  | None -> Alcotest.fail "should be some data"
  | Some (s, _) -> Alcotest.(check string "data is good" "002000mo002001mn001002nm001003nl001004nk" s)

let tests = [
  "empty reassembly queue", `Quick, empty_is_empty ;
  "non-empty reassembly queue", `Quick, added_is_nonempty ;
  "added can be taken", `Quick, added_can_be_taken ;
  "added can be taken 2", `Quick, added_can_be_taken2 ;
  "added can be taken 3", `Quick, added_can_be_taken3 ;
  "coalescing works", `Quick, coalescing_works ;
  "coalescing works rev", `Quick, coalescing_works_rev ;
  "coalescing works 3", `Quick, coalescing_works_3 ;
  "coalescing works 4", `Quick, coalescing_works_4 ;
  "coalescing works 5", `Quick, coalescing_works_5 ;
  "coalescing works 6", `Quick, coalescing_works_6 ;
  "coalescing works 7", `Quick, coalescing_works_7 ;
  "take works", `Quick, take_works ;
  "take works taking before", `Quick, take_works_taking_before ;
  "take works taking before 2", `Quick, take_works_taking_before_2 ;
  "overlap 1", `Quick, overlap_1 ;
  "overlap 2", `Quick, overlap_2 ;
  "overlap 3", `Quick, overlap_3 ;
  "overlap 4", `Quick, overlap_4 ;
  "overlap 5", `Quick, overlap_5 ;
  "overlap 6", `Quick, overlap_6 ;
  "regression 187", `Quick, regr_187 ;
  "regression 188", `Quick, regr_188 ;
]
