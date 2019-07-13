(* (c) 2019 Hannes Mehnert, all rights reserved *)

(* see TCP1_timersScript.sml, but we don't need to be fuzzy *)

type 'a timed = 'a * Mtime.t

let timer now a duration =
  match Mtime.add_span now (Mtime.Span.of_uint64_ns duration) with
  | None -> assert false (* ?? *)
  | Some x -> a, x

let timer_expired now (v, deadline) =
  if Mtime.is_later ~than:deadline now then
    Some v
  else
    None

let time_pass_stopwatch now (_, start) = Mtime.span now start
