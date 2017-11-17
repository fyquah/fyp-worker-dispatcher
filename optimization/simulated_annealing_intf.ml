open Core
open Async

type config =
  { t_max          : float;
    t_min          : float;
    updates        : int;
    steps          : int;
    workers        : int;
  }
[@@deriving sexp_of]

module Step = struct
  type ('state, 'energy) t =
    { initial         : ('state * 'energy);
      proposal        : ('state * 'energy);
      step            : int;
      decision        : [ `Accepted | `Rejected ]
    }
  [@@deriving sexp]
end

module type T = sig
  type state [@@deriving sexp]

  type energy [@@deriving sexp]

  val energy : state -> energy Deferred.Or_error.t

  val move : step: int -> config: config -> state -> state Deferred.Or_error.t

  val energy_to_float : energy -> float

  include Comparable.S with type t := state
end

module type S = sig

  module T : sig
    include T
  end

  type t =
    { state          : T.state;
      current_energy : T.energy;
      step           : int;
      accepts        : int;
      improves       : int;

      energy_cache   : T.energy Deferred.t T.Map.t;
      best_solution  : (T.state * T.energy);

      config         : config;
    }
  [@@deriving sexp_of]

  module Step : sig
    type t = (T.state, T.energy) Step.t [@@deriving sexp]
  end

  val empty : ?config: config -> T.state -> T.energy -> t

  val step : t -> (Step.t * t) Deferred.t
end
