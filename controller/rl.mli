open Core
open Common

module S : sig
  type t [@@deriving compare, sexp]

  include Comparable.S with type t := t

  val make : unit -> t

  val terminal : t

  val to_string : t -> string

  val is_terminal : t -> bool

  val pprint : t -> string
end

module A : sig
  type t = Data_collector.V1.Action.t = Inline | Specialise | Apply
  [@@deriving compare, sexp]

  include Comparable.S with type t := t
end

module Trajectory : sig
  type 'a t =
    { entries: (S.t * A.t) list;
      terminal_state: S.t;
      reward: float;
      metadata: 'a;
    }
  [@@deriving sexp]
end

module Pending_trajectory : sig
  type t = ((S.t * A.t) list * S.t) [@@deriving sexp]
end

type transition = S.t -> A.t -> S.t

module MCTS : sig
  type t

  val init : rollout_policy: (S.t -> A.t) -> t

  val mk_policy : t -> (S.t -> A.t option) Staged.t

  val rollout_policy : t -> (S.t -> A.t) Staged.t

  val backprop : t -> trajectory: 'a Trajectory.t -> t

  val expand : t -> path: (S.t * A.t) list -> t
end
