[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common

module SA = Optimization.Simulated_annealing

module Annealer = struct
  module T = struct
    type state = Inlining_tree.t [@@deriving sexp]

    type t = state [@@deriving sexp]

    let compare = Inlining_tree.compare
  end

  include T
  include Comparable.Make(T)

  let move ~(step : int) ~(config: SA.Common.config) state =
    ignore step;
    ignore config;
    let num_leaves = Inlining_tree.Top_level.count_leaves state in
    let choice = Random.int num_leaves in
    Inlining_tree.Top_level.flip_several_leaves state [ choice ]
  ;;
end

let command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"Command"
    [%map_open
      let {
        Command_params.
        config_filename;
        controller_rundir;
        exp_dir;
        bin_name;
        bin_args } = Command_params.params
      in
      fun () ->
        Deferred.Or_error.return ()
    ]
