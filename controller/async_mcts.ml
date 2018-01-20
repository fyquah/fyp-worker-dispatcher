open Core
open Async
open Common

module RL = Rl
module EU = Experiment_utils
module Work_unit = EU.Work_unit


let run_one_iteration
    ~(root_state: RL.S.t)
    ~(transition: RL.transition)
    ~(mcts: RL.MCTS.t)
    ~(generate_work_unit: (RL.Pending_trajectory.t -> Work_unit.t Deferred.Or_error.t))
    ~(execute_work_unit: EU.Work_unit.t -> Execution_stats.t Deferred.Or_error.t)
    ~(reward_of_exec_time: Time.Span.t -> float) =

  let rec loop state ~policy ~acc =
    let action = policy state in
    let next_state = transition state action in
    let acc = (state, action) :: acc in
    if RL.S.is_terminal next_state then
      (next_state, (List.rev acc))
    else
      loop next_state ~policy ~acc
  in

  (* phase 1, Choose action using MCTS *)
  let mcts_terminal, mcts_trajectory =
    let policy = Staged.unstage (RL.MCTS.mk_policy mcts) in
    loop root_state ~policy ~acc:[]
  in

  (* phase 2, 3, use the [rollout_policy] *)
  let rollout_terminal, rollout_trajectory =
    let policy = Staged.unstage (RL.MCTS.rollout_policy mcts) in
    loop mcts_terminal ~policy ~acc:[]
  in
  let trajectory_entries = mcts_trajectory @ rollout_trajectory in
  generate_work_unit (trajectory_entries, rollout_terminal)
  >>=? fun work_unit ->
  execute_work_unit work_unit
  >>|? fun execution_time ->
  let reward = reward_of_exec_time (gmean_exec_time execution_time) in
  { RL.Trajectory.
    entries  = trajectory_entries;
    terminal_state = rollout_terminal;
    reward = reward;
  }

  (* Phase 4 (backprop) is executed asynchronously *)
;;

let learn
    ~(parallelism: int)
    ~num_iterations
    ~(root_state: RL.S.t)
    ~(transition: RL.transition)
    ~(rollout_policy: RL.S.t -> RL.A.t)
    ~(compile_binary: (RL.Pending_trajectory.t -> string Deferred.Or_error.t))
    ~(execute_work_unit: EU.Work_unit.t -> Execution_stats.t Deferred.Or_error.t)
    ~(reward_of_exec_time: Time.Span.t -> float) =
  let mcts_ref = ref (RL.MCTS.init ~rollout_policy) in
  List.init num_iterations ~f:Fn.id
  |> Deferred.Or_error.List.iter ~how:(`Max_concurrent_jobs parallelism)
      ~f:(fun iter ->
        let generate_work_unit trajectory =
          compile_binary trajectory >>|? fun path_to_bin ->
          { Work_unit. path_to_bin; step = 0; sub_id = 0; }
        in
        lift_deferred (Clock.after (Time.Span.of_sec (Random.float 3.0)))
        >>=? fun () ->
        run_one_iteration ~root_state ~transition ~mcts:!mcts_ref
            ~generate_work_unit ~execute_work_unit ~reward_of_exec_time
        >>|? fun trajectory ->
        mcts_ref := RL.MCTS.backprop !mcts_ref ~trajectory)
;;
