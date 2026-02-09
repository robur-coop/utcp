type g = unit

let block = 1

let create ?time:_ () = ()

let generate_into ~g:_ data ~off len =
  for i = off to off + len - 1 do
    Bytes.set data i '\x00'
  done

let reseed ~g:_ _bytes = ()

let accumulate ~g:_ _source = `Acc (fun _data -> ())

let seeded ~g:_ = true

let pools = 1
