open Core
open Async

module Common = struct
  type config = Simulated_annealing_intf.config =
    { t_max             : float;
      t_min             : float;
      updates           : int;
      steps             : int;
      workers           : int;
    }
  [@@deriving sexp_of]

  let temperature (config : config) step =
    let step = Float.of_int step in
    let steps = Float.of_int config.steps in
    let t_factor = -1.0 *. Float.log (config.t_max /. config.t_min) in
    config.t_max *. Float.exp (t_factor *. step /. steps)
  ;;
end

module Make(T: Simulated_annealing_intf.T) = struct

  type t =
    { state          : T.state;
      current_energy : T.energy;
      step           : int;
      accepts        : int;
      improves       : int;

      energy_cache   : T.energy Deferred.t T.Map.t;
      best_solution  : T.state * T.energy;

      config         : Common.config;
    }
  [@@deriving sexp_of]

  let sexp_of_t t =
    sexp_of_t { t with energy_cache = T.Map.empty }
  ;;

  module Step = struct
    type t = (T.state, T.energy) Simulated_annealing_intf.Step.t
    [@@deriving sexp]
  end

  let default_config =
    { Common.
      t_max = 0.04;
      t_min = 0.0005;
      updates = 100;
      steps = 300;
      workers = 1;
    }

  let empty ?(config = default_config) state current_energy =
    { state;
      current_energy;
      step = 0;
      accepts = 0;
      improves = 0;

      energy_cache = T.Map.empty;
      best_solution = (state, current_energy);

      config;
    }

  let query_energy t s =
    match T.Map.find t.energy_cache s with
    | None   ->
      Log.Global.sexp ~level:`Info
        [%message "Computing energy for state "
           (s : T.state)];
      (* TODO: ok_exn is NOT okay *)
      let deferred_energy = T.energy t.state >>| ok_exn in
      let energy_cache =
        T.Map.add t.energy_cache ~key:s ~data:deferred_energy
      in
      { t with energy_cache }, deferred_energy
    | Some deferred_energy ->
      Log.Global.sexp ~level:`Info
        [%message "Loading energy from cache for state "
           (s : T.state)];
      t, deferred_energy
  ;;

  let bump_best_solution t state energy =
    let (best_soln, best_energy) = t.best_solution in
    if T.energy_to_float energy <. T.energy_to_float best_energy
    then { t with best_solution = state, energy }
    else t
  ;;

  let step t =
    let t_step = t.step in
    Log.Global.sexp ~level:`Info
      [%message (t_step : int) "Calling step" (t : t)];
    let temperature = Common.temperature t.config t.step in
    let current_state = t.state in
    let current_energy = t.current_energy in
    assert (t.config.workers >= 1);
    (* TODO: ok_exn here is NOT okay! *)
    let%bind next_state =
      T.move ~config:t.config ~step:t.step current_state
      >>| ok_exn
    in
    let t, next_energy = query_energy t next_state in
    let%bind next_energy = next_energy in
    let t = bump_best_solution t current_state current_energy in
    let t = bump_best_solution t next_state next_energy in
    let d_e =
      T.energy_to_float next_energy -. T.energy_to_float current_energy
    in
    let rand = Random.float 1.0 in
    let probability =
      if d_e <= 0.0 then 1.0
      else Float.exp (Float.neg d_e /. temperature)
    in
    let step = t.step in
    Log.Global.sexp ~level:`Info
      [%message
        (step : int)
        (probability : float)
        (rand : float)
        (current_energy : T.energy)
        (next_energy : T.energy)];
    if d_e > 0.0 && probability <. rand then begin
      Log.Global.sexp ~level:`Info [%message (step : int) "Rejecting change!"];
      let step =
        { Simulated_annealing_intf.Step.
          initial  = (current_state, current_energy);
          proposal = (next_state, next_energy);
          step     = t.step;
          decision = `Rejected
        }
      in
      return (step, { t with step = t.step + 1 })
    end else begin
      let accepts = t.accepts + 1 in
      let improves = if d_e <. 0.0 then t.improves + 1 else t.improves in
      let step_description =
        { Simulated_annealing_intf.Step.
          initial  = (current_state, current_energy);
          proposal = (next_state, next_energy);
          step     = t.step;
          decision = `Accepted
        }
      in
      let state = next_state in
      Log.Global.sexp ~level:`Info
        [%message (step: int) "Accepting change to new state!"];
      return (
        step_description,
        { t with state; step = t.step + 1; accepts; improves;
                      current_energy = next_energy; })
    end
  ;;
end
