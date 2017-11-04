[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

module Results = Protocol.Results

type simple_stats = { mean : Time.Span.t; sd : Time.Span.t; }

let compute_time_stats raw_execution_time =
  let seconds =
    Array.of_list_map ~f:Time.Span.to_sec raw_execution_time
  in
  let mean = Time.Span.of_sec (Gsl.Stats.mean seconds) in
  let sd = Time.Span.of_sec (Gsl.Stats.sd seconds) in
  { mean; sd; }

let command_compare_decisions =
  let open Command.Let_syntax in
  Command.async' ~summary:"Compare decisions"
  [%map_open
   let filename_1 = anon ("filename_1" %: string)
   and filename_2 = anon ("filename-2" %: string)
   in
   let load_sexp filename =
     Reader.load_sexp_exn filename [%of_sexp: Results.t]
   in
   let render_boolean name flag =
     if flag then
       printf "==> %s equal\n" name
     else
       printf "==> %s NOT EQUAL!\n" name
   in
   fun () ->
     let open Deferred.Let_syntax in
     Deferred.both (load_sexp filename_1) (load_sexp filename_2)
     >>= fun (sexp_1, sexp_2) ->
     let decisions_equal =
       List.equal ~equal:Fyp_compiler_lib.Data_collector.equal
         sexp_1.decisions sexp_2.decisions
     in
     let overrides_equal =
       List.equal ~equal:Fyp_compiler_lib.Data_collector.equal
         sexp_1.overrides sexp_2.overrides
     in
     let print_time { mean; sd; } =
       sprintf !"%{Time.Span} (+-%{Time.Span})" mean sd
     in
     render_boolean "decisions" decisions_equal;
     render_boolean "overrides" overrides_equal;
     printf !"==> Exec time : %{print_time} VS %{print_time}\n"
       (compute_time_stats sexp_1.benchmark.raw_execution_time)
       (compute_time_stats sexp_2.benchmark.raw_execution_time);
     Deferred.unit
  ]

let command_display_time =
  Command.async' ~summary:"Display"
    (Command.Param.return
       (fun () ->
         let pipe = Reader.read_sexps (Lazy.force Reader.stdin) in
         Pipe.iter pipe ~f:(fun sexp ->
             let results = [%of_sexp: Results.t] sexp in
             let work_unit_id = results.Results.id in
             let benchmark = results.benchmark in
             let { mean; sd; } =
               compute_time_stats benchmark.raw_execution_time
             in
             let print_worker_hostname = function
               | Some hostname -> hostname
               | None -> "<NONE>"
             in
             printf !"%{Results.Work_unit_id},%{print_worker_hostname},%{Time.Span},%{Time.Span}\n"
               work_unit_id benchmark.worker_hostname mean sd;
             Deferred.unit)))

let () =
  Command.group ~summary:"Display"
    [("display-time", command_display_time);
     ("compare-results", command_compare_decisions);
    ]
  |> Command.run
