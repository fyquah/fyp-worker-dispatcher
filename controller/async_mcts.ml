open Core
open Async
open Common

module RL = Rl
module EU = Experiment_utils
module Work_unit = EU.Work_unit


let zip_with_delay l =
  match l with
  | [] -> []
  | _ :: tl ->
    List.zip_exn l ((List.map ~f:(fun x -> Some x) tl) @ [None])
;;

let log_trajectory ~iter ~trajectory ~terminal =
  let pprint_action = function
    | RL.A.Inline -> "INLINE"
    | RL.A.No_inline -> "BRANCH"
  in
  let messages = 
    zip_with_delay trajectory
    |> List.map ~f:(fun ((s, a), s') ->
        let s' =
          match s' with
          | Some x -> fst x
          | None -> terminal
        in
        sprintf !"%{RL.S.pprint} ---%{pprint_action}--> %{RL.S.pprint}" s a s')
  in
  let messages =
    if List.length messages <= 10 then
      messages
    else
      let front = List.take messages 5 in
      let back = List.rev (List.take (List.rev messages) 5) in
      let pruned = List.length messages - 10 in
      front @ [ sprintf "... (Prunted %d transitions) ..." pruned  ] @ back
  in
  List.iter messages ~f:(fun msg -> Log.Global.info "[iter %d] %s" iter msg)
;;


let plan_and_simulate ~iter_id
    ~(root_state: RL.S.t)
    ~(transition: RL.transition)
    ~(mcts: RL.MCTS.t)
    ~(generate_work_unit: (RL.Pending_trajectory.t -> Work_unit.t Deferred.Or_error.t))
    ~(execute_work_unit: EU.Work_unit.t -> Execution_stats.t Deferred.Or_error.t)
    ~(reward_of_exec_time: Time.Span.t -> float) =

  let rec loop state ~policy ~acc =
    if RL.S.is_terminal state then
      (state, (List.rev acc))
    else
      let action = policy state in
      match action with 
      | None -> 
        (state, List.rev acc)
      | Some action ->
        let next_state = transition state action in
        let acc = (state, action) :: acc in
        loop next_state ~policy ~acc
  in

  (* TODO(fyq14): SOMETHING VERY VERY WRONG HERE !!! The current thing will
   * cause MCTS to fully expand to TERM, which is clearly not 
   * the intended behaviour. We want to expand only up to a leaf node in the
   * Q-value estimator.
   *)

  Log.Global.info "[iteration %d] MCTS expanding" iter_id;

  (* phase 1, Choose action using MCTS *)
  let mcts_terminal, mcts_trajectory =
    let policy = Staged.unstage (RL.MCTS.mk_policy mcts) in
    loop root_state ~policy ~acc:[]
  in
  log_trajectory ~iter:iter_id ~trajectory:mcts_trajectory ~terminal:mcts_terminal;

  Log.Global.info "[iteration %d] Simulating" iter_id;

  (* phase 2, 3, use the [rollout_policy] *)
  let rollout_terminal, rollout_trajectory =
    let policy x = Some (Staged.unstage (RL.MCTS.rollout_policy mcts) x) in
    loop mcts_terminal ~policy ~acc:[]
  in
  log_trajectory ~iter:iter_id ~trajectory:rollout_trajectory ~terminal:rollout_terminal;

  let trajectory_entries = mcts_trajectory @ rollout_trajectory in
  let pending_trajectory = (trajectory_entries, rollout_terminal) in
  (RL.MCTS.expand mcts ~path:trajectory_entries, pending_trajectory)
;;

let run_single_iteration ~iter_id
    ~pending_trajectory
    ~(generate_work_unit: (RL.Pending_trajectory.t -> Work_unit.t Deferred.Or_error.t))
    ~(execute_work_unit: EU.Work_unit.t -> Execution_stats.t Deferred.Or_error.t)
    ~(reward_of_exec_time: Time.Span.t -> float) =

  generate_work_unit pending_trajectory
  >>=? fun work_unit ->
  execute_work_unit work_unit
  >>|? fun execution_stats ->
  let reward = reward_of_exec_time (gmean_exec_time execution_stats) in
  let trajectory = 
    { RL.Trajectory.
      entries  = fst pending_trajectory;
      terminal_state = snd pending_trajectory;
      reward = reward;
      metadata = execution_stats;
    }
  in
  Log.Global.info !"Iteration %d (time: %{Time.Span}) (reward: %.3f)" \
    iter_id (gmean_exec_time execution_stats) reward;
  trajectory
;;

let learn
    ~(parallelism: int)
    ~num_iterations
    ~(root_state: RL.S.t)
    ~(transition: RL.transition)
    ~(rollout_policy: RL.S.t -> RL.A.t)
    ~(compile_binary: (RL.Pending_trajectory.t -> string Deferred.Or_error.t))
    ~(execute_work_unit: EU.Work_unit.t -> Execution_stats.t Deferred.Or_error.t)
    ~(reward_of_exec_time: Time.Span.t -> float)
    ~record_trajectory =
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
        let (mcts, pending_trajectory) =
          plan_and_simulate ~iter_id:iter ~root_state ~transition ~mcts:!mcts_ref
              ~generate_work_unit ~execute_work_unit ~reward_of_exec_time
        in
        mcts_ref := mcts;
        run_single_iteration ~iter_id:iter ~pending_trajectory
            ~generate_work_unit ~execute_work_unit ~reward_of_exec_time
        >>=? fun trajectory ->
        (* We backprop AS SOON AS POSSIBLE so other threads can pick up our
         * change
         *)
        mcts_ref := RL.MCTS.backprop !mcts_ref ~trajectory;
        record_trajectory ~iter trajectory
      )
;;
