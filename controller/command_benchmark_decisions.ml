open Core
open Async
open Common

module Config = Protocol.Config
module EU = Experiment_utils

let command_run =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"Command"
    [%map_open
      let {
        Command_params.
        config_filename;
        controller_rundir;
        exp_dir;
        bin_name;
        bin_args;
       } = Command_params.params
      and overrides_file = flag "-overrides" (required file) ~doc:"Hello"
      and should_print_csv = flag "-csv" no_arg ~doc:"Machine parsable" 
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind config =
          Reader.load_sexp config_filename [%of_sexp: Config.t]
        in
        let num_runs = 6 in
        let%bind worker_connections =
          Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
            ~f:(fun worker_config ->
              let hostname = Protocol.Config.hostname worker_config in
              Experiment_utils.init_connection ~hostname ~worker_config)
        in
        let%bind path_to_bin =
          let write_overrides dest =
            shell ~dir:exp_dir "cp" [ overrides_file; dest; ]
          in
          let dump_directory =
            EU.Dump_utils.execution_dump_directory ~step:`Initial
              ~sub_id:`Current
          in
          EU.compile_binary ~dir:exp_dir ~bin_name ~write_overrides
            ~dump_directory
        in
        let%bind results =
          Deferred.Or_error.List.mapi ~how:`Parallel worker_connections
            ~f:(fun step conn ->
              let dump_dir =
                Experiment_utils.Dump_utils.execution_dump_directory
                  ~step:`Initial ~sub_id:(`Sub_id step)
              in
              lift_deferred (Async_shell.mkdir ~p:() dump_dir)
              >>=? fun () ->
              let processor = EU.Worker_connection.processor conn in
              let hostname = EU.Worker_connection.hostname conn in
              let identifier =
                sprintf "%s_%d" hostname (Option.value_exn processor)
              in
              Experiment_utils.run_binary_on_worker
                ~processor ~num_runs ~conn ~path_to_bin
                ~hostname ~bin_args ~dump_dir
              >>|? fun execution_stats ->
              (identifier, execution_stats))
        in
        List.iter results ~f:(fun (identifier, execution_stats) ->
          let raw_execution_time =
            List.map execution_stats.raw_execution_time
              ~f:Time.Span.to_string_hum
            |> String.concat ~sep:", "
          in
          let gmean_exec_time =
            List.map execution_stats.raw_execution_time
              ~f:Time.Span.to_sec
            |> Common.geometric_mean
            |> Time.Span.of_sec
          in
          if not should_print_csv then begin
            printf "Execution [%s]\n" identifier;
            printf " - path_to_bin = %s\n" path_to_bin;
            printf " - raw execution_time = %s\n" raw_execution_time;
            printf !" - gmean raw execution time = %{Time.Span.to_string_hum}\n"
              gmean_exec_time
          end else begin
            let raw_execution_time =
              List.map execution_stats.raw_execution_time
                ~f:Time.Span.to_string_hum
            in
            identifier :: overrides_file :: controller_rundir :: raw_execution_time
            |> String.concat ~sep:","
            |> printf "%s\n"
          end
        );
        Deferred.Or_error.ok_unit]

;;

let command =
  Command.group ~summary:"Benchmark decisions"
    [("run", command_run)]
;;
