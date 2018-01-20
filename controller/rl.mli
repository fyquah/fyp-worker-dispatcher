open Core
open Common

module S : sig
  type t [@@deriving compare, sexp]

  include Comparable.S with type t := t

  val make : unit -> t

  val to_string : t -> string
end

module A : sig
  type t = Inline | No_inline [@@deriving compare, sexp]

  include Comparable.S with type t := t
end

module Trajectory : sig
  type t =
    { entries: (S.t * A.t) list;
      terminal_state: S.t;
      reward: float;
    }
  [@@deriving sexp]
end

module Pending_trajectory : sig
  type t = ((S.t * A.t) list * S.t) [@@deriving sexp]
end

type transition = S.t -> A.t -> [`Leaf of S.t | `Node of S.t ]

module MCTS : sig
  type t

  val init : rollout_policy: (S.t -> A.t) -> t

  val mk_policy : t -> (S.t -> A.t) Staged.t

  val rollout_policy : t -> (S.t -> A.t) Staged.t

  val backprop : t -> trajectory: Trajectory.t -> t
end
