open Crowbar

let buf = dynamic_bind (range 256) bytes_fixed
let pp ppf = Format.fprintf ppf "%04x"

let () =
  add_test ~name:"checksum" [ buf ] @@ fun buf ->
  let { Cstruct.buffer; off; len } = Cstruct.of_string buf in
  let a = Utcp.Checksum.unsafe_digest_16 ~sum:0 ~off ~len buffer in
  let b = Utcp.Checksum.unsafe_digest_32 ~sum:0 ~off ~len buffer in
  check_eq ~pp a b
