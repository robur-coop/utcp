(* (c) 2019 Hannes Mehnert, all rights reserved *)

let src = Logs.Src.create "tcp.timer_helper" ~doc:"TCP timer helpers"
module Log = (val Logs.src_log src : Logs.LOG)

(* see TCP1_timersScript.sml, but we don't need to be fuzzy *)

type 'a timed = 'a * Mtime.t

let timer now a duration =
  let span =
    Mtime.add_span now (Mtime.Span.of_uint64_ns duration)
  in
  if span = None then
    Log.warn (fun m -> m "span overflow (now %a duration %Lu)"
                 Mtime.pp now duration);
  a, Option.value ~default:Mtime.max_stamp span

let timer_expired now (v, deadline) =
  if Mtime.is_later ~than:deadline now then
    Some v
  else
    None

let time_pass_stopwatch now (_, start) = Mtime.span now start
