open Core
open Async

module Make(T: sig
  type state

  val energy : state -> float Deferred.t

  val move : state -> state Deferred.t

  include Comparable.S with type t := state

end) = struct

  type state = T.state

  type best_solution =
    { energy         : float;
      state          : state;
    }

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

      best_solution  : best_solution option;
      energy_cache   : float Deferred.t T.Map.t;

      config         : config;
    }

  let default_config =
    { t_max = 25_000.0;
      t_min = 2.5;
      updates = 100;
      steps = 1000;
    }

  let empty state =
    { state;
      step = 0;
      accepts = 0;
      improves = 0;

      best_solution = None;
      energy_cache = T.Map.empty;

      config = default_config;
    }

  let query_energy t s =
    match T.Map.find t.energy_cache s with
    | None   ->
      let deferred_energy = T.energy t.state in
      let energy_cache =
        T.Map.add t.energy_cache ~key:s ~data:deferred_energy
      in
      { t with energy_cache }, deferred_energy
    | Some deferred_energy ->
      t, deferred_energy
  ;;

  let step t =
    let config = t.config in
    let t_factor = -1.0 *. Float.log (config.t_max /. config.t_min) in
    let sensitivity =
      let steps = Float.of_int config.steps in
      let step = Float.of_int t.step in
      config.t_max *. Float.exp (t_factor *. step /. steps)
    in
    let current_state = t.state in
    let t, current_energy = query_energy t current_state in
    let%bind next_state = T.move current_state in
    let t, next_energy = query_energy t next_state in
    let%bind (current_energy, next_energy) =
      Deferred.both current_energy next_energy
    in
    let d_e = next_energy -. current_energy in
    let rand = Random.float 1.0 in
    if d_e > 0.0 && Float.exp (Float.neg d_e /. sensitivity) <. rand then
      { t with step = t.step + 1 }
    else
      let accepts = t.accepts + 1 in
      let improves = if d_e <. 0.0 then t.improves + 1 else t.improves in
      let step = t.step + 1 in
      { t with step; accepts; improves; }
  ;;
end
