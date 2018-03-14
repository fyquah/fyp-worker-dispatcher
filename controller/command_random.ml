open Core
open Async
open Common

module Config = Protocol.Config
module EU = Experiment_utils
module Inlining_tree = Protocol.Inlining_tree.V1


let total_steps = 200


(* TODO(fyq14): How to be really really unbiased here? *)
let pertubate_tree (tree : Inlining_tree.Top_level.t) =
  Inlining_tree.Top_level.uniform_random_mutation tree
;;


let command_run =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"Run random sampling for things."
    [%map_open
      let {
        Command_params.
        config_filename;
        controller_rundir = _;
        exp_dir;
        bin_name;
        bin_args;
       } = Command_params.params
      in
      fun () ->
        Reader.load_sexp config_filename [%of_sexp: Config.t]
        >>=? fun config ->
        Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
          ~f:(fun worker_config ->
            let hostname = Protocol.Config.hostname worker_config in
            Experiment_utils.init_connection ~hostname ~worker_config)
        >>=? fun worker_connections ->

        (* TODO(fyq14): get initial state from a different sample instead? *)
        EU.get_initial_state ~bin_name ~exp_dir ~base_overrides:[] ()
        >>=? fun initial_state ->
        let initial_state = Option.value_exn initial_state in
        Deferred.Or_error.return ()
        >>=? fun () ->
        Experiment_utils.copy_compilation_artifacts
          ~exp_dir ~abs_path_to_binary:initial_state.path_to_bin
          ~dump_dir:(
            Experiment_utils.Dump_utils.execution_dump_directory
              ~step:`Initial ~sub_id:`Current)
        >>=? fun () ->
        let process =
          let num_runs = config.num_runs in
          Experiment_utils.process_work_unit ~num_runs ~bin_args 
        in
        lift_deferred (EU.Scheduler.create worker_connections ~process)
        >>=? fun scheduler ->

        let execute_tree ~step ~sub_id tree =
          let dump_directory =
            EU.Dump_utils.execution_dump_directory ~step ~sub_id
          in
          let write_overrides output_filename =
            let overrides =
              Inlining_tree.Top_level.to_override_rules tree
            in
            lift_deferred (
              Writer.save_sexp output_filename
                ([%sexp_of: Data_collector.Overrides.t] overrides)
            )
          in
          Experiment_utils.compile_binary ~dir:exp_dir ~bin_name
            ~dump_directory ~write_overrides
          >>=? fun path_to_bin ->
          let work_unit = { EU.Work_unit. step; sub_id; path_to_bin; } in
          Experiment_utils.Scheduler.dispatch scheduler work_unit
        in
        let initial_tree = Inlining_tree.build initial_state.v1_decisions in
        let cache = ref (
          Inlining_tree.Top_level.Map.of_alist_exn []
        )
        in
        Deferred.repeat_until_finished (0, initial_tree) (fun (step, previous_tree) ->
          let tree = pertubate_tree previous_tree in
          begin
            match Inlining_tree.Top_level.Map.find !cache tree with
            | Some exc_stats ->
              Log.Global.info "[Step %d] Load tree from cache" step;
              Deferred.Or_error.return exc_stats
            | None ->
              Log.Global.info "[Step %d] Dispatching to worker" step;
              execute_tree ~step:(`Step step) ~sub_id:`Current tree
              >>=? fun exc_stats ->
              cache := (Inlining_tree.Top_level.Map.add !cache tree exc_stats);
              Deferred.Or_error.return exc_stats
          end
          >>= function
          | Ok (_ : Execution_stats.t) ->
            begin
            if step < total_steps then
              Deferred.return (`Repeat (step + 1, tree))
            else
              Deferred.return (`Finished (Ok ()))
            end
          | Error e -> Deferred.return (`Finished (Error e)))]


let command =
  Command.group ~summary:"Random sampling"
    [("run",  command_run)]
;;

