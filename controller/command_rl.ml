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

let command =
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
        Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
          ~f:(fun worker_config ->
            let hostname = Protocol.Config.hostname worker_config in
            Experiment_utils.init_connection ~hostname ~worker_config)
        >>=? fun worker_connections ->
        let env = [("OCAMLPARAM", "_,exhaustive-inlining=1")] in
        Experiment_utils.get_initial_state ~env ~bin_name ~exp_dir
            ~base_overrides:[] ()
        >>=? fun initial_state ->
        let initial_state = Option.value_exn initial_state in
        let inlining_tree = Inlining_tree.build initial_state.decisions in
        let cfg = Cfg.t_of_inlining_tree inlining_tree in
        let random_policy = fun (_ : RL.S.t) ->
          let r = Random.int 2 in
          if r = 0 then
            RL.A.Inline
          else if r = 1 then
            RL.A.No_inline
          else
            assert false
        in

        let process = Experiment_utils.process_work_unit ~config ~bin_args in
        lift_deferred (EU.Scheduler.create worker_connections ~process)
        >>=? fun scheduler ->

        EU.run_in_all_workers ~times:3 ~scheduler ~config ~initial_state
        >>=? fun initial_execution_stats ->

        let exec_time = gmean_exec_time initial_execution_stats in
        let reward_of_exec_time (time : Time.Span.t) =
          (* The relative difference in performance *)
          let slowdown =
            Time.Span.to_sec exec_time /. Time.Span.to_sec time
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
            Nano_mutex.lock_exn lock;
            EU.compile_binary ~dir:exp_dir ~bin_name:bin_name (
              Cfg.overrides_of_pending_trajectory cfg pending_trajectory)
            >>|? fun s ->
            Nano_mutex.unlock_exn lock;
            s
        in
        let execute_work_unit work_unit =
          EU.Scheduler.dispatch scheduler work_unit
        in
        let transition state action = Cfg.transition cfg state action in
        Async_mcts.learn ~parallelism:(List.length worker_connections)
          ~num_iterations:num_iterations
          ~root_state:cfg.root
          ~transition
          ~rollout_policy:random_policy  (* TODO(fyq14): Use Flambda's default? But how? *)
          ~compile_binary
          ~execute_work_unit
          ~reward_of_exec_time
    ]
  ;;
