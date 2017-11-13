open Core
open Async

module type T = sig
  type state

  val energy : state -> float Deferred.t

  val move : state -> state Deferred.t

  include Comparable.S with type t := state
end

module type S = sig

  module T : sig
    include T
  end

  type state = T.state
  
  type config = 
    { t_max          : float;
      t_min          : float;
      updates        : int;
      steps          : int;
    }
  
  type t =
    { state          : state;
      step           : int;
      accepts        : int;
      improves       : int;
  
      energy_cache   : float Deferred.t T.Map.t;
  
      config         : config;
    }
  
  val empty : state -> t
  
  val step : t -> t Deferred.t
end

