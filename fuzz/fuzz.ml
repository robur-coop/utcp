open Crowbar

let buf = dynamic_bind (range 256) bytes_fixed
let pp ppf = Format.fprintf ppf "%04x"

let () =
  add_test ~name:"checksum" [ buf ] @@ fun buf ->
  let { Cstruct.buffer; off; len } = Cstruct.of_string buf in
  let a = Utcp.Checksum.unsafe_feed_16_le ~off ~len 0 buffer in
  let b = Utcp.Checksum.unsafe_feed_32_le ~off ~len 0 buffer in
  let a = Utcp.Checksum.finally a in
  let b = Utcp.Checksum.finally b in
  check_eq ~pp a b
