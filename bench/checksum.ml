open Bechamel
open Toolkit

let payload =
  let ln = 3_000 in
  let ba = Bigarray.(Array1.create char c_layout ln) in
  Bigarray.Array1.fill ba '\xff'; ba

let checksum = Staged.stage @@ fun () ->
  ignore (Utcp.Checksum.digest payload)

let test =
  Test.make ~name:"checksum" checksum

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
