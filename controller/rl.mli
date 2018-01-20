open Core
open Common

module S : sig
  type t [@@deriving compare, sexp]

  include Comparable.S with type t := t

  val make : unit -> t
end

module A : sig
  type t = Inline | No_inline [@@deriving compare, sexp]

  include Comparable.S with type t := t
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
end


module Trajectory : sig
  type t =
    { entries: (S.t * A.t) list;
      terminal_state: S.t;
      reward: float;
    }
  [@@deriving sexp]
end

type transition = S.t -> A.t -> [`Leaf of S.t | `Node of S.t ]

