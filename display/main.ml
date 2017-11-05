[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

module Results = Protocol.Results

type simple_stats = { mean : Time.Span.t; sd : Time.Span.t; }

let load_results_sexp filename =
  Reader.load_sexp_exn filename [%of_sexp: Results.t]
;;

let compute_time_stats raw_execution_time =
  let seconds =
    Array.of_list_map ~f:Time.Span.to_sec raw_execution_time
  in
  let mean = Time.Span.of_sec (Owl.Stats.mean seconds) in
  let sd = Time.Span.of_sec (Owl.Stats.std seconds) in
  { mean; sd; }
;;

let command_print_time_to_python_list =
  let open Command.Let_syntax in
  Command.async' ~summary:"summary"
  [%map_open
   let filename = anon ("filename" %: string) in
   fun () ->
     load_results_sexp filename
     >>| fun results ->
     results.benchmark.raw_execution_time
     |> List.map ~f:(fun t -> Float.to_string (Time.Span.to_sec t))
     |> String.concat ~sep:", "
     |> printf "[ %s ]"
  ]
;;

let command_merge_results =
  let open Command.Let_syntax in
  Command.async' ~summary:"Merge results"
  [%map_open
   let filename_1 = anon ("filename" %: string)
   and filename_2 = anon ("filename" %: string) in
   fun () ->
     let open Deferred.Let_syntax in
     Deferred.both
       (load_results_sexp filename_1) (load_results_sexp filename_2)
     >>= fun (results_1, results_2) ->
     let decisions_equal =
       List.equal ~equal:Fyp_compiler_lib.Data_collector.equal
         results_1.decisions results_2.decisions
     in
     let overrides_equal =
       List.equal ~equal:Fyp_compiler_lib.Data_collector.equal
         results_1.overrides results_2.overrides
     in
     if not decisions_equal || not overrides_equal then begin
       failwith "Cannot merge results with non equal decisions and overrides"
     end;
     let benchmark =
       let raw_execution_time =
         results_1.benchmark.raw_execution_time
         @ results_2.benchmark.raw_execution_time
       in
       let worker_hostname = None in
       { Protocol.Execution_stats. worker_hostname; raw_execution_time; }
     in
     let merged = { results_1 with path_to_bin = None; benchmark } in
     let stdout = Lazy.force Writer.stdout in
     Writer.write_sexp stdout ([%sexp_of: Results.t] merged);
     Deferred.unit
  ]

let command_compare_exec_time =
  let open Command.Let_syntax in
  Command.async' ~summary:"T-test on execution time"
  [%map_open
   let filename_1 = anon ("filename" %: string)
   and filename_2 = anon ("filename" %: string) in
   fun () ->
     let open Deferred.Let_syntax in
     let load_sexp filename =
       Reader.load_sexp_exn filename [%of_sexp: Results.t]
     in
     Deferred.both (load_sexp filename_1) (load_sexp filename_2)
     >>= fun (result_a, result_b) ->
     let extract_seconds (res : Results.t) =
       Array.of_list_map res.benchmark.raw_execution_time ~f:(fun time ->
           Time.Span.to_sec time)
     in
     let seconds_a = extract_seconds result_a in
     let seconds_b = extract_seconds result_b in
     let side = Owl.Stats.BothSide in
     let alpha = 0.05 in
     let (h, p, _t) =
       Owl.Stats.t_test_unpaired ~equal_var:false ~alpha ~side
         seconds_a seconds_b
     in
     (* [h] refers to whether they are from different distributions *)
     printf "h = %b p = %.3f\n" h p;
     Deferred.unit
  ]
;;

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
     let print_worker_hostname = function
       | None -> "NONE"
       | Some s -> s
     in
     render_boolean "decisions" decisions_equal;
     render_boolean "overrides" overrides_equal;
     printf "==> Worker hostname %s %s\n"
       (print_worker_hostname sexp_1.benchmark.worker_hostname)
       (print_worker_hostname sexp_2.benchmark.worker_hostname);

     begin match sexp_1.path_to_bin, sexp_2.path_to_bin with
     | None, _
     | _, None ->
       printf "==> binaries: Cannot compare!\n";
       Deferred.unit
     | Some a, Some b ->
       Monitor.try_with
         (fun () -> (Async_shell.run ~expect:[0] "diff" [ a; b; ]))
       >>| begin function
       | Ok () ->  true
       | Error _exn -> false
       end
       >>| fun flag -> render_boolean "binaries" flag
     end
     >>= fun () ->
     let exec_time_1 = sexp_1.benchmark.raw_execution_time in
     let exec_time_2 = sexp_2.benchmark.raw_execution_time in
     printf !"==> Exec time : %{print_time} VS %{print_time}\n"
       (compute_time_stats exec_time_1) (compute_time_stats exec_time_2);
     let alpha = 0.05 in
     let side = Owl.Stats.BothSide in
     let (h, p, _) =
       Owl.Stats.t_test_unpaired ~equal_var:false ~alpha ~side
         (Array.of_list_map ~f:Time.Span.to_sec exec_time_1)
         (Array.of_list_map ~f:Time.Span.to_sec exec_time_2)
     in
     printf !"==> t-test : %b %.3f\n" h p;

     Deferred.unit
  ]

let command_plot_exec_time =
  let open Command.Let_syntax in
  Command.async' ~summary:"Plot"
    [%map_open
     let filename = anon ("filename" %: string) in
     fun () ->
       load_results_sexp filename
       >>= fun results ->
       let extract_seconds (res : Results.t) =
         Array.of_list_map res.benchmark.raw_execution_time ~f:(fun time ->
             Time.Span.to_sec time)
       in
       let seconds = extract_seconds results in
       Owl.Plot.histogram (
         Bigarray.Array2.of_array
           Bigarray.float64
           Bigarray.c_layout
           (Array.create ~len:1 seconds)
       );
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
     ("diff-results", command_compare_decisions);
     ("compare-exec-time", command_compare_exec_time);
     ("merge-results", command_merge_results);
     ("print-time-to-python-list", command_print_time_to_python_list);
     ("plot", command_plot_exec_time);
    ]
  |> Command.run
;;
