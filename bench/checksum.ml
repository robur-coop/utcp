open Bechamel
open Toolkit

let payload =
  let ln = 3_000 in
  let ba = Bigarray.(Array1.create char c_layout ln) in
  Bigarray.Array1.fill ba '\xff'; ba

let checksum = Test.make ~name:"0xff" @@ Staged.stage @@ fun () ->
  ignore (Utcp.Checksum.digest ~off:0 ~len:3000 payload)

let real = Cstruct.of_hex
  {|00 50 d0 c5 90 3b 6f b9 31 8e 90 da 70 12 ff ff
    83 98 00 00 02 04 05 b4 03 03 06 00|}

let src = Ipaddr.(V4 (V4.of_string_exn "10.0.42.2"))
and dst = Ipaddr.(V4 (V4.of_string_exn "10.0.42.1"))

let segment = Test.make ~name:"segment" @@ Staged.stage @@ fun () ->
  ignore (Utcp.Segment.(checksum ~src ~dst real))

let test = Test.make_grouped ~name:"checksum" [ checksum; segment ]

let benchmark () =
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |] in
  let instances = Instance.[ monotonic_clock ] in
  let cfg = Benchmark.cfg ~limit:2000 ~stabilize:true ~quota:(Time.second 1.) ~kde:(Some 1000) () in
  let raw = Benchmark.all cfg instances test in
  let results = List.map (fun inst -> Analyze.all ols inst raw) instances in
  let results = Analyze.merge ols instances results in
  (results, raw)

let nothing _ = Ok ()

let () =
  let results = benchmark () in
  let results =
    let open Bechamel_js in
    emit ~dst:(Channel stdout) nothing ~x_label:Measure.run
      ~y_label:(Measure.label Instance.monotonic_clock) results in
  match results with Ok () -> () | Error (`Msg err) -> failwith err
