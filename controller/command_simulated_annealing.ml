[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common

module Utils = Experiment_utils
module SA = Optimization.Simulated_annealing

module Work_unit = struct
  type t = string [@@deriving sexp]
end

module Make_annealer(M: sig

  val initial_execution_time : Time.Span.t

  val scheduler :
    (Work_unit.t,
     Execution_stats.t Or_error.t,
     Socket.Address.Inet.t) Utils.Scheduler.t

  val exp_dir : string

  val bin_name : string

end) = struct
  module T = struct
    type state =
      { tree        : Inlining_tree.Top_level.t;
        work_unit   : Work_unit.t;
      }
    [@@deriving sexp]

    type t = state [@@deriving sexp]

    (* We don't want debugging messages to contain the entire tree. *)
    let sexp_of_state state = Work_unit.sexp_of_t state.work_unit

    let compare a b = List.compare Inlining_tree.compare a.tree b.tree
  end

  module T2 = struct
    include T
    include Comparable.Make(T)

    let unique_random_from_list ~count choices =
      let rec loop ~choices ~(left : int) =
        if Int.O.(left <= 0) then []
        else begin
          let size = Int.Set.length choices in
          let selected =
            Option.value_exn (Int.Set.nth choices (Random.int size))
          in
          selected :: loop ~left:(left - 1) ~choices:(Int.Set.remove choices selected)
        end
      in
      loop ~choices:(Int.Set.of_list choices) ~left:count
    ;;

    let move ~(step : int) ~(config: SA.Common.config) state =
      ignore step;
      ignore config;
      let current_tree = state.tree in
      let num_leaves = Inlining_tree.Top_level.count_leaves current_tree in
      let choices =
        (* flip between 1 to 3 leaves *)
        unique_random_from_list
          ~count:(Int.min num_leaves (Random.int 3 + 1))
          (List.init num_leaves ~f:Fn.id)
      in
      let new_tree =
        Inlining_tree.Top_level.flip_several_leaves current_tree choices
      in
      shell ~dir:M.exp_dir "make" [ "clean" ]
      >>=? fun () ->
      let overrides = Inlining_tree.Top_level.to_override_rules new_tree in
      lift_deferred (
        Writer.save_sexp (M.exp_dir ^/ "overrides.sexp")
          ([%sexp_of: Data_collector.t list] overrides)
      )
      >>=? fun () ->
      shell ~dir:M.exp_dir "make" [ "all" ]
      >>=? fun () ->
      let filename = Filename.temp_file "fyp-" ("-" ^ M.bin_name) in
      shell ~dir:M.exp_dir
        "cp" [ (M.bin_name ^ ".native"); filename ]
      >>=? fun () ->
      shell ~dir:M.exp_dir "chmod" [ "755"; filename ]
      >>=? fun () ->
      Reader.load_sexp (M.exp_dir ^/ (M.bin_name ^ ".0.data_collector.sexp"))
        [%of_sexp: Data_collector.t list]
      >>=? fun executed_decisions ->
      (* TODO: This is incredibly expensive -- there is a lot of potential
       * for tree structure sharing here.
       *)
      let tree = Inlining_tree.build executed_decisions in
      let work_unit = filename in
      Deferred.Or_error.return { T. tree; work_unit; }
    ;;

    let energy state =
      let num_workers = Experiment_utils.Scheduler.num_workers M.scheduler in
      Deferred.Or_error.List.init num_workers ~how:`Parallel ~f:(fun _ ->
        Experiment_utils.Scheduler.dispatch M.scheduler state.work_unit)
      >>|? fun results ->
      let mean_exec_time =
        geometric_mean (
          List.concat_map results ~f:(fun r -> r.raw_execution_time)
          |> List.map ~f:Time.Span.to_sec
        )
      in
      Log.Global.sexp ~level:`Info [%message
        (state : T.state)
        (mean_exec_time : float)];
      mean_exec_time /. Time.Span.to_sec M.initial_execution_time
    ;;
  end

  include SA.Make(T2)
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
        Log.Global.sexp ~level:`Info [%message "building initial state" ];
        Utils.get_initial_state ~bin_name ~exp_dir ~base_overrides:[] ()
        >>=? fun initial_state ->
        let initial_state = Option.value_exn initial_state in
        let process conn work_unit =
          let path_to_bin = work_unit in
          let num_runs =
            config.num_runs / List.length config.worker_configs
          in
          Experiment_utils.run_binary_on_worker
            ~num_runs ~config ~conn ~path_to_bin
            ~hostname:(Utils.Worker_connection.hostname conn)
            ~bin_args
        in
        lift_deferred (Utils.Scheduler.create worker_connections ~process)
        >>=? fun scheduler ->
        (* We run this 3 more times than the others to gurantee
         * stability of the distribution of initial execution times.
         *)
        Deferred.Or_error.List.init ~how:`Sequential 3 ~f:(fun i ->
          Log.Global.sexp ~level:`Info
            [%message "Initial state run " (i : int)];
          Utils.Scheduler.dispatch scheduler initial_state.path_to_bin)
        >>=? fun initial_execution_times ->
        let initial_execution_time =
          List.concat_map initial_execution_times ~f:(fun stat ->
            List.map ~f:Time.Span.to_sec stat.raw_execution_time)
          |> geometric_mean
          |> Time.Span.of_sec
        in
        printf !"Initial Execution Time = %{Time.Span}\n"
          initial_execution_time;
        let module Annealer = Make_annealer(struct
          let bin_name = bin_name
          let exp_dir = exp_dir
          let initial_execution_time = initial_execution_time
          let scheduler = scheduler
        end)
        in
        let state =
          let tree = initial_state.traversal_state.tree_root in
          let work_unit = initial_state.path_to_bin in
          let initial = { Annealer.T. tree; work_unit; } in
          let t = Annealer.empty initial in
          let energy_cache =
            Annealer.T2.Map.add t.energy_cache
              ~key:initial ~data:(Deferred.return 1.0)
          in
          { t with energy_cache }
        in
        Deferred.repeat_until_finished state (fun state ->
          let sexp = Annealer.sexp_of_t state in
          printf "Step %d:\n%s\n" state.step (Sexp.to_string_hum sexp);
          Annealer.step state
          >>| fun next -> `Repeat next
        )
    ]
