open Core
open Async

module Make(T: Simulated_annealing_intf.T
  ) : Simulated_annealing_intf.S with module T := T
