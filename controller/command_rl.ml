open Core
open Async
open Common

(* The availble algorithms in RL are as follows:
 *
 * MCTS_WITH_IRL
 *   policy <- initial_sensible_policy
 *
 *   loop until timeout {
 *     trajectories <- sample N trajectories with MCTS using policy
 *     learn reward function using trajectories (basically, do IRL)
 *     policy <- solve optimal policy using reward function (with simple DP)
 *   }
 *
 *   result <- argmax(policy)
 *
 * Asynchronous MCTS
 *   MCTS as described in AlphaGo's paper, but without using value or policy
 *   networks
 *)

module RL = Rl  (* RL is much nicer alias than Rl :) *)
module Cfg = Cfg
module EU = Experiment_utils
module Async_MCTS = Async_mcts


let random_policy = fun (_ : RL.S.t) ->
  let r = Random.int 2 in
  if r = 0 then
    RL.A.Inline
  else if r = 1 then
    RL.A.No_inline
  else
    assert false
;;

let command_run =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"Command"
    [%map_open
      let { Command_params.
        config_filename; controller_rundir; exp_dir; bin_name; bin_args;
      } = Command_params.params
      and log_rewards =
        flag "-log-rewards" (required bool) ~doc:"BOOL logarithmic speedups as reward"
      and num_iterations =
        flag "-num-iterations" (required int) ~doc:"INT Number of iterations"
      in
      fun () ->
        Reader.load_sexp config_filename [%of_sexp: Config.t]
        >>=? fun config ->
        Log.Global.sexp ~level:`Info [%message
          "Initializing Worker connections!"
            (config: Config.t)];
        Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
          ~f:(fun worker_config ->
            let hostname = Protocol.Config.hostname worker_config in
            Experiment_utils.init_connection ~hostname ~worker_config)
        >>=? fun worker_connections ->
        let env = [("OCAMLPARAM", "_,exhaustive-inlining=1")] in

        Log.Global.sexp ~level:`Info [%message
          "Initializing Worker connections!"
            (config: Config.t)];
        Experiment_utils.get_initial_state ~env ~bin_name ~exp_dir
            ~base_overrides:[] ()
        >>=? fun initial_state ->

        let initial_state = Option.value_exn initial_state in

        Log.Global.info "Constructing inlining tree";
        let inlining_tree = Inlining_tree.build initial_state.decisions in

        Log.Global.info "Constructing CFG";
        let cfg = Cfg.t_of_inlining_tree inlining_tree in

        Log.Global.info "Saving CFG and inlining tree to rundir";
        lift_deferred (
          Writer.save_sexp (controller_rundir ^/ "cfg.sexp")
            ([%sexp_of: Cfg.t] cfg)
        )
        >>=? fun () ->
        lift_deferred (
          Writer.save_sexp (controller_rundir ^/ "inlining_tree.sexp")
            ([%sexp_of: Inlining_tree.Top_level.t] inlining_tree))
        >>=? fun () ->

        (* It is okay to run just twice (or even once?), I believe.
         *
         * MCTS should be smart enough to retry due to sampling errors.
         * *)
        let process = Experiment_utils.process_work_unit ~num_runs:2 ~bin_args in
        lift_deferred (EU.Scheduler.create worker_connections ~process)
        >>=? fun scheduler ->

        EU.compile_binary ~dir:exp_dir ~bin_name:bin_name []
        >>=? fun basic_path_to_bin ->

        EU.run_in_all_workers ~times:3 ~scheduler ~config ~path_to_bin:basic_path_to_bin
        >>=? fun initial_execution_stats ->

        let baseline_exec_time = gmean_exec_time initial_execution_stats in
        let reward_of_exec_time (time : Time.Span.t) =
          (* The relative difference in performance *)
          let slowdown =
             Time.Span.to_sec time /. Time.Span.to_sec baseline_exec_time
          in
          if log_rewards then
            (-1.0) *. (Float.log slowdown)
          else
            (-1.0) *. slowdown
        in
        let compile_binary =
          (* Lock required because we don't want to processes to be
           * compiling code at the same time -- it'd cause a recase in
           * the available data structures
           *)
          let lock = Nano_mutex.create () in
          fun pending_trajectory ->
            Deferred.repeat_until_finished () (fun () ->
              match Nano_mutex.lock lock with
              | Ok () -> Deferred.return (`Finished (Or_error.return ()))
              | Error _ ->  Deferred.return (`Repeat ()))
            >>=? fun () ->
            EU.compile_binary ~dir:exp_dir ~bin_name:bin_name (
              Cfg.overrides_of_pending_trajectory cfg pending_trajectory)
            >>=? fun s ->
            Deferred.repeat_until_finished () (fun () ->
              match Nano_mutex.unlock lock with
              | Ok () -> Deferred.return (`Finished (Or_error.return ()))
              | Error _ ->  Deferred.return (`Repeat ()))
            >>|? fun () ->
            s
        in
        let execute_work_unit work_unit =
          EU.Scheduler.dispatch scheduler work_unit
        in
        let transition state action =
          let next = Cfg.transition cfg state action in
          Log.Global.info !"| %{RL.S} -> %{RL.S}" state next;
          next
        in
        let record_trajectory ~iter (trajectory: Execution_stats.t RL.Trajectory.t) =
          let dirname = controller_rundir ^/ "opt_data" ^/ Int.to_string iter in
          let filename = dirname ^/ "trajectory.sexp" in
          lift_deferred (Async_shell.mkdir ~p:() dirname)
          >>=? fun () ->
          lift_deferred (
            Writer.save_sexp filename ([%sexp_of: Execution_stats.t RL.Trajectory.t] trajectory)
          )
        in
        Async_mcts.learn ~parallelism:(List.length worker_connections)
          ~num_iterations:num_iterations
          ~root_state:cfg.root
          ~transition
          ~rollout_policy:random_policy  (* TODO(fyq14): Use Flambda's default? But how? *)
          ~compile_binary
          ~execute_work_unit
          ~reward_of_exec_time
          ~record_trajectory
    ]
;;

let command = Command.group ~summary:"" [("run", command_run)]
