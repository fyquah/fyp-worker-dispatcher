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

module RL = struct
  module S = struct 
    type t = Closure_id.t [@@deriving compare, sexp]
  end

  module A = struct
    type t = Inline | No_inline [@@deriving compare, sexp]
  end

  module SA_pair = struct
    module T = struct
      type t = (S.t * A.t) [@@deriving compare, sexp]
    end

    include T

    module Map = Map.Make(T)
  end

  module MCTS : sig 
    type t

    val empty : t

    val mk_policy : t -> (S.t -> A.t) Staged.t

    val backprop
       : t
       -> trajectory: (S.t * A.t) list
       -> terminal: S.t
       -> reward: float
       -> t
  end = struct

    type value =
      { total_reward: float;
        visits:       int;
      }

    type t = value SA_pair.Map.t

    let alpha = 0.01

    let empty = SA_pair.Map.empty

    let mk_policy t =
      let delta = 0.02 in (* probability that we go wrong *)
      let estimate_value (v: value) =
        if v.visits = 0 then
          1000000.0 (* a proxy for inifinity *)
        else
        let q = v.total_reward /. float_of_int v.visits in
        let upper_bound =
          (* TODO(fyq14): What is the formula for UCB? *)
          Float.sqrt (
            (2. /. float_of_int v.visits) *. Float.log (1. /. delta)
          )
        in
        q +. upper_bound
      in
      Staged.stage (fun s ->
          let default = { total_reward = 0.0; visits = 0 } in
          let inline =
            Option.value ~default (SA_pair.Map.find t (s, A.Inline))
          in
          let no_inline =
            Option.value ~default (SA_pair.Map.find t (s, A.No_inline))
          in
          let inline_value = estimate_value inline in
          let no_inline_value = estimate_value no_inline in
          if inline_value >. no_inline_value then
            A.Inline
          else
            A.No_inline)
    ;;

    let backprop t ~trajectory ~terminal ~reward =
      let rec loop trajectory ~acc =
        match trajectory with
        | hd :: tl ->
          begin match SA_pair.Map.find acc hd with 
          | None ->
            SA_pair.Map.add acc ~key:hd
              ~data:{ total_reward = reward; visits = 1 }
          | Some entry -> 
            let update_entry { total_reward; visits; } =
              { total_reward = total_reward +. reward; visits = visits + 1; }
            in
            let entry = update_entry entry in
            let updated = SA_pair.Map.add acc ~key:hd ~data:entry in
            loop tl ~acc:updated
          end
        | [] -> acc
      in
      loop trajectory ~acc:t
    ;;
  end

  module Trajectory = struct
    type t =
      { entries: (S.t * A.t) list;
        terminal_state: S.t;
        reward: float;
      }
    [@@deriving sexp]
  end
end


type transition = RL.S.t -> RL.A.t -> [`Leaf of RL.S.t | `Node of RL.S.t ]

let mcts_loop
    ~(root_state: RL.S.t)
    ~(transition: transition)
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


let command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"Command"
    (Command.Param.return (fun () ->
      let random_policy = fun (_ : RL.S.t) ->
        let r = Random.int 2 in
        if r = 0 then
          RL.A.Inline
        else if r = 1 then
          RL.A.No_inline
        else
          assert false
      in
      Deferred.Or_error.return ()))
