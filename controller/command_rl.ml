open Core
open Async
open Common

(* The algorithm vaguely corresponds to the following:
 *
 * policy <- random_selection()
 *
 * loop until timeout {
 *   trajectories <- sample N trajectories with MCTS using policy
 *   learn reward function using trajectories (basically, do IRL)
 *   policy <- solve optimal policy using reward function (with simple DP)
 * }
 *
 * result <- argmax(policy)
 *)

module RL = Rl  (* RL is much nicer alias than Rl :) *)
module Cfg = Cfg
module EU = Experiment_utils

let mcts_loop
    ~(root_state: RL.S.t)
    ~(transition: RL.transition)
    ~(rollout_policy: RL.S.t -> RL.A.t)
    ~(mcts: RL.MCTS.t)
    ~(reward_of_exec_time: Time.Span.t -> float) =

  let rec loop state ~policy ~acc =
    let action = policy state in
    match transition state action with
    | `Leaf terminal_state ->
      (terminal_state, (List.rev ((state, action) :: acc)))
    | `Node next_state ->
      loop next_state ~policy ~acc:((state, action) :: acc)
  in

  (* phase 1, Choose action using MCTS *)
  let mcts_terminal, mcts_trajectory =
    let policy = Staged.unstage (RL.MCTS.mk_policy mcts) in
    loop root_state ~policy ~acc:[]
  in

  (* phase 2, 3, use the [rollout_policy] *)
  let rollout_terminal, rollout_trajectory =
    let policy = rollout_policy in
    loop mcts_terminal ~policy ~acc:[]
  in

  (* TODO(fyquah): Actually execute code! *)
  let%bind execution_time =
    Deferred.return (Time.Span.of_sec 2.0)
  in
  let reward = reward_of_exec_time execution_time in
  let entries = mcts_trajectory @ rollout_trajectory in

  (* phase 4: Backprop MCTS *)
  let mcts =
    RL.MCTS.backprop mcts ~trajectory:entries ~reward
      ~terminal:rollout_terminal
  in
  let trajectory =
    { RL.Trajectory.
      entries  = entries;
      terminal_state = rollout_terminal;
      reward = reward;
    }
  in
  Deferred.return (mcts, trajectory)
;;

module Opt_method = struct
  type t =
    | MCTS
  [@@deriving sexp]
end

let command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"Command"
    [%map_open
      let { Command_params.
        config_filename; controller_rundir; exp_dir; bin_name; bin_args;
      } = Command_params.params
      and opt_method =
        flag "-method" (required string) ~doc:"STRING optimization method"
      and log_rewards =
        flag "-log-rewards" (required bool) ~doc:"BOOL logarithmic speedups as reward"
      in
      fun () ->
        Reader.load_sexp config_filename [%of_sexp: Config.t]
        >>=? fun config ->
        let opt_method =
          Sexp.of_string_conv_exn opt_method Opt_method.t_of_sexp
        in
        Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
          ~f:(fun worker_config ->
            let hostname = Protocol.Config.hostname worker_config in
            Experiment_utils.init_connection ~hostname ~worker_config)
        >>=? fun worker_connections ->
        Experiment_utils.get_initial_state ~bin_name ~exp_dir
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

        EU.run_in_all_workers ~scheduler ~config ~bin_args
            ~worker_connections ~initial_state
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
        Deferred.Or_error.return ()
    ]
  ;;
