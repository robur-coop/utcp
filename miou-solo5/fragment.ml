let src = Logs.Src.create "fragment"

module Log = (val Logs.src_log src : Logs.LOG)

type t =
  | Unsized : Ropes.unknown Ropes.t -> t
  | Sized : Diet.t * bytes -> t

(* A payload can be:
   - an entire packet (Unfragmented)
   - the start of a fragment whose end is unknown (Unsized)
   - a packet that has not yet been fully received (but which size is known) (Sized)

   In the case of a fragment whose size is unknown, a Rope is used so that
   insertion at random locations is not costly. This Rope is in a state where we
   do not know the final size ([Ropes.unknown]). We can therefore add fragments
   one after the other "ad infinitum". However, we cannot add a fragment on top
   of another existing one. If this is the case, the addition raises the
   [Overlap] exception.

   Then, if we know the last fragment, we know the final size of our packet. We
   can therefore [Ropes.fixed] our ropes. This consists of creating a final buffer
   in which we will copy all our fragments (a reassembly, in short). However,
   this does not mean that we have received the whole of our package; there may
   be holes.

   It is therefore always necessary to check that the addition of new fragments
   to this buffer does not overlap other already existing fragments. For this
   reason, we associate with this buffer a Discrete Interval Encoding Tree that
   contains all the intervals already "filled" in our buffer. A [Sized] value is
   then created.

   Thanks to the DIET, we can verify that the addition of all these intervals
   forms a continuous final interval of our packet in its entirety. All we have
   to do is make a [Diet.diff] between our current DIET and the one that
   contains this final interval. If the result is empty, it means that our
   current DIET contains all the intervals necessary to complete our buffer. Our
   packet is complete!

   Finally, there is the special case of an unfragmented packet. This
   corresponds to our "happy path" where the bigstring used physically
   corresponds to the one used (and filled) by Solo5. That is to say that from
   Solo5 up to here, this unfragmented packet **is not** a copy.
*)

let singleton ~off ?(limit= false) slice : t =
  match off, limit with
  | _, false ->
    let str = Slice_bstr.to_string slice in
    let empty = Ropes.(Unknown Limitless) in
    let ropes = Ropes.insert ~off str empty in
    Log.debug (fun m -> m "+%d byte(s) %@ %d" (String.length str) off);
    Unsized ropes
  | 0, true -> invalid_arg "Unfragmented packet"
  | _, true ->
    let str = Slice_bstr.to_string slice in
    let empty = Ropes.(Unknown Limitless) in
    let ropes = Ropes.insert ~off str empty in
    let max = off + String.length str in
    Log.debug (fun m -> m "+%d byte(s) %@ %d (max: %d)" (String.length str) off max);
    let ropes = Ropes.fixed ~max ropes in
    let diet, buf = Ropes.to_bytes ropes in
    Sized (diet, buf)

let weight = function
  | Unsized ropes -> Ropes.weight ropes
  | Sized (_, buf) -> Bytes.length buf

exception Out_of_bounds
exception Overlap
exception Too_big

let () = Printexc.register_printer @@ function
  | Out_of_bounds -> Some "Fragment out of bounds"
  | Overlap -> Some "Fragment overlap"
  | Too_big -> Some "Too big"
  | _ -> None

let insert t ~off ?(limit= false) str =
  match t, limit with
  | Sized (diet, buf), false ->
      let len = String.length str in
      if off < 0
      || off > Bytes.length buf - len
      then raise_notrace Out_of_bounds;
      begin try
        let diet = Diet.add ~off ~len diet in
        Bytes.unsafe_blit_string str 0 buf off len;
        Sized (diet, buf)
      with _ -> raise_notrace Overlap end
  | Unsized ropes, false ->
      Log.debug (fun m -> m "+%d byte(s) %@ %d" (String.length str) off);
      let ropes = Ropes.insert ~off str ropes in
      (* NOTE(dinosaure): actually, we  can increase the ropes without limit.
         This is also the case with [mirage-tcpip], which has no mechanism to
         limit the [payload]. Not sure if you should put a limit or not. *)
      Unsized ropes
  | Sized _, true -> failwith "Multiple MF:0 fragments"
  | Unsized ropes, true ->
      let max = off + String.length str in
      Log.debug (fun m -> m "+%d byte(s) %@ %d (max: %d)" (String.length str) off max);
      let ropes = Ropes.insert ~off str ropes in
      let ropes = Ropes.fixed ~max ropes in
      let diet, buf = Ropes.to_bytes ropes in
      Sized (diet, buf)

let is_complete : t -> bool = function
  | Unsized _ -> false
  | Sized (diet, buf) ->
      let buf = Diet.add ~off:0 ~len:(Bytes.length buf) Diet.empty in
      Diet.(is_empty (diff diet buf))

let reassemble_exn = function
  | Unsized _ -> invalid_arg "Fragment.reassemble_exn"
  | Sized (_, buf) -> Bytes.unsafe_to_string buf
