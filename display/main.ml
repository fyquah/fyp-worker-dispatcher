[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

module Results = Protocol.Results

let () =
  Command.async' ~summary:"Display"
    (Command.Param.return
       (fun () ->
         let pipe = Reader.read_sexps (Lazy.force Reader.stdin) in
         Pipe.iter pipe ~f:(fun sexp ->
             let results = [%of_sexp: Results.t] sexp in
             let work_unit_id = results.Results.id in
             let benchmark = results.benchmark in
             let seconds =
               benchmark.raw_execution_time
               |> Array.of_list_map ~f:Time.Span.to_sec
             in
             let mean = Time.Span.of_sec (Gsl.Stats.mean seconds) in
             let sd = Time.Span.of_sec (Gsl.Stats.sd seconds) in
             printf !"%{Results.Work_unit_id},%{Time.Span},%{Time.Span}\n"
               work_unit_id mean sd;
             Deferred.unit)))
  |> Command.run
