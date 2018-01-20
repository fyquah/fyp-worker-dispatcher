open Core
open Common

module S = struct
  module T = struct
    type t = int [@@deriving compare, sexp]

    let to_string = Int.to_string
  end

  let make =
    let r = ref (-1) in
    fun () ->
      r := !r + 1;
      !r
  ;;

  include T
  include Comparable.Make(T)
end

module A = struct
  module T = struct
    type t = Inline | No_inline [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make(T)
end

module SA_pair = struct
  module T = struct
    type t = (S.t * A.t) [@@deriving compare, sexp]
  end

  include T

  module Map = Map.Make(T)
end

module MCTS = struct

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

module Pending_trajectory = struct
  type t = ((S.t * A.t) list * S.t) [@@deriving sexp]
end


type transition =
  S.t -> A.t -> [`Leaf of S.t | `Node of S.t ]
