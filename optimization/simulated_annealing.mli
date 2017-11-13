open Core
open Async

module Common : sig
  type config = Simulated_annealing_intf.config

  val temperature : config -> int -> float
end

module Make(T: Simulated_annealing_intf.T
  ) : Simulated_annealing_intf.S with module T := T
