[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common

module Utils = Experiment_utils
module SA = Optimization.Simulated_annealing

type performance =
  { mu : float;
    sigma : float;
    raw_execution_time: float array;
  }

module Work_unit = struct
  type t = string (* Path to binary *)
end

module Make_annealer(M: sig

  val initial_performance : performance

  val scheduler : (Work_unit.t * Execution_stats.t * unit) Utils.Scheduler.t

end) = struct
  module T = struct
    type state =
      { tree        : Inlining_tree.t;
        work_unit   : Work_unit.t;
      }
      [@@deriving sexp]

    type t = state [@@deriving sexp]

    let compare a b = Inlining_tree.compare a.tree b.tree
  end

  include T
  include Comparable.Make(T)

  let move ~(step : int) ~(config: SA.Common.config) state =
    ignore step;
    ignore config;
    let num_leaves = Inlining_tree.Top_level.count_leaves state in
    let choice = Random.int num_leaves in
    let new_tree =
      Inlining_tree.Top_level.flip_several_leaves state [ choice ]
    in
    shell ~echo:true ~verbose:true ~dir:exp_dir "make" [ "clean" ]
    >>=? fun () ->
    let overrides = Inlining_tree.Top_level.to_override_rules new_tree in
    lift_deferred (
      Writer.save_sexp (exp_dir ^/ "overrides.sexp")
        ([%sexp_of: Data_collector.t list] overrides)
    )
    >>=? fun () ->
    shell ~verbose:true ~dir:exp_dir "make" [ "all" ]
    >>=? fun () ->
    let filename = Filename.temp_file "fyp-" ("-" ^ bin_name) in
    shell ~echo:true ~verbose:true ~dir:exp_dir
      "cp" [ (bin_name ^ ".native"); filename ]
    >>=? fun () ->
    shell ~echo:true ~dir:exp_dir "chmod" [ "755"; filename ]
    >>=? fun () ->
    Reader.load_sexp (exp_dir ^/ (bin_name ^ ".0.data_collector.sexp"))
    >>=? fun executed_decisions ->
    (* TODO: This is incredibly expensive -- there is a lot of potential
     * for tree structure sharing here.
     *)
    let tree = Inlining_tre.build executed_decisions in
    Deferred.Or_error.return { T. tree; filename; }
  ;;

  let energy state =
    let%map result = Scheduler.dispatch M.scheduler state.work_unit in
    (* TODO: geometric mean *)
    Owl.Stats.mean (
      Array.map ~f:(fun t -> t /. M.initial_performance)
        result.raw_execution_time
    )
  ;;
end

let command =
  let open Command.Let_syntax in
  Command.async' ~summary:"Command"
    [%map_open
      let {
        Command_params.
        config_filename;
        controller_rundir;
        exp_dir;
        bin_name;
        bin_args;
       } = Command_params.params
      in
      fun () ->
        Reader.load_sexp config_filename [%of_sexp: Config.t]
        >>=? fun config ->
        Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
          ~f:(fun worker_config ->
            let hostname = Protocol.Config.hostname worker_config in
            Experiment_utils.init_connection ~hostname ~worker_config)
        >>=? fun worker_connections ->
        Utils.get_initial_state ~bin_name ~exp_dir ~base_overrides:[] ()
        >>=? fun initial_state ->
        let _, initial_state = Option.value_exn initial_state in
        let initial_state = initial_state.tree_root in

        let module Annealer = SA.Make(Make_annealer()) in

        let process conn work_unit =
          let path_to_bin = work_unit in
          Experiment_utils.run_binary_on_worker ~config ~conn ~path_to_bin
            ~hostname:(Worker_connection.hostname conn)
            ~bin_args
          >>=

          Log.Global.info ">>>= Running generation %d" generation.gen;
        in
        let%bind scheduler =
          Utils.Scheduler.create worker_connections ~process
        in
        Deferred.Or_error.return ()
    ]
