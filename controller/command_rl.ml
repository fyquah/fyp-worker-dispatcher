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
  let%bind execution_time = Deferred.return (Time.Span.of_sec 2.0) in
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
      let common_params = Command_params.params
      and opt_method =
        flag "-method" (required string) ~doc:"STRING optimization method"
      in
      fun () ->
        let opt_method =
          Sexp.of_string_conv_exn opt_method Opt_method.t_of_sexp
        in
        let random_policy = fun (_ : RL.S.t) ->
          let r = Random.int 2 in
          if r = 0 then
            RL.A.Inline
          else if r = 1 then
            RL.A.No_inline
          else
            assert false
        in
        match opt_method with
        | MCTS -> Deferred.Or_error.return ()
    ]
