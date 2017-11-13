[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common

module State = struct
  module T = struct
    type state = Inlining_tree.t [@@deriving sexp]

    type t = state [@@deriving sexp]
    
    let compare = Inlining_tree.compare
  end

  include T
  include Comparable.Make(T)

  let move inlining_tree =
  ;;
end

module SA = Optimization.Simulated_annealing.Make(State)

let command =
  let open Command.Let_syntax in
  Command.async_or_error' ~doc:"Command"
    [%map_open
      let {
        Command_params.
        config_filename;
        controller_rundir;
        exp_dir;
        bin_name;
        bin_args } = Command_params.params
      in
      Deferred.Or_error.return ()
    ]
