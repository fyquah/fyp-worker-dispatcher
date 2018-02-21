[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common

module Utils = Experiment_utils
module SA = Optimization.Simulated_annealing
module Work_unit = Experiment_utils.Work_unit
module Perf_stats = Execution_stats.Perf_stats

module Base_state_energy = struct
  module V0 = struct
    type energy = Execution_stats.t [@@deriving sexp]

    type state =
      { tree        : Inlining_tree.V0.Top_level.t;
        work_unit   : Work_unit.t;
      }
    [@@deriving sexp]

    type t = state [@@deriving sexp]

    (* We don't want debugging messages to contain the entire tree. *)
    let sexp_of_state state = Work_unit.sexp_of_t state.work_unit

    let state_of_sexp sexp =
      { tree = []; work_unit = Work_unit.t_of_sexp sexp; }
    ;;
  end

  module V1 = struct
    type energy = V0.energy [@@deriving sexp]

    type state =
      { tree        : Inlining_tree.V1.Top_level.t;
        work_unit   : Work_unit.t;
      }
    [@@deriving sexp]

    type t = state [@@deriving sexp]

    (* We don't want debugging messages to contain the entire tree. *)
    let sexp_of_state state = Work_unit.sexp_of_t state.work_unit

    let state_of_sexp sexp =
      { tree = []; work_unit = Work_unit.t_of_sexp sexp; }
    ;;
  end

  include V1
end

module Step = struct
  module V0 = struct
    type state = Base_state_energy.V0.state [@@deriving sexp]
    type energy = Base_state_energy.V0.energy [@@deriving sexp]

    type t = (state, energy) Optimization.Simulated_annealing_intf.Step.t
    [@@deriving sexp]
  end

  module V1 = struct
    type state = Base_state_energy.V1.state [@@deriving sexp]
    type energy = Base_state_energy.V1.energy [@@deriving sexp]

    type t = (state, energy) Optimization.Simulated_annealing_intf.Step.t
    [@@deriving sexp]
  end

  include V1
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
  module T1 = struct
    include Base_state_energy

    let compare a b = List.compare Inlining_tree.V1.compare a.tree b.tree
  end

  module T2 = struct
    include T1
    include Comparable.Make(T1)

    let energy_to_float (e : energy) =
      let a =
        geometric_mean (List.map e.raw_execution_time ~f:Time.Span.to_sec)
      in
      a /. Time.Span.to_sec M.initial_execution_time
    ;;

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

    let move ~(step : int) ~(sub_id: int) ~(config: SA.Common.config) state =
      ignore step;
      ignore config;
      let current_tree = state.tree in
      let num_leaves = Inlining_tree.V1.Top_level.count_leaves current_tree in
      (* flip between 1 to 3 leaves *)
      let new_tree =
        let rec make () =
          if Random.bool () then
            let modified_leaves = 1 in
            let choices =
              unique_random_from_list ~count:modified_leaves
                (List.init num_leaves ~f:Fn.id)
            in
            Inlining_tree.V1.Top_level.flip_several_leaves current_tree choices
          else
            let leaf = Random.int num_leaves in
            match
              Inlining_tree.V1.Top_level.backtrack_nth_leaf current_tree leaf
            with
            | None ->
              Log.Global.sexp ~level:`Info [%message "Failed to backtrack!"];
              make ()
            | Some transformed ->
              Log.Global.sexp ~level:`Info [%message "Back track possible!!"];
              transformed
        in
        make ()
      in
      shell ~dir:M.exp_dir "make" [ "clean" ]
      >>=? fun () ->
      let overrides =
        Inlining_tree.V1.Top_level.to_override_rules new_tree
      in
      lift_deferred (
        Writer.save_sexp (M.exp_dir ^/ "overrides.sexp")
          ([%sexp_of: Data_collector.V1.Overrides.t] overrides)
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
      let data_collector_file =
        M.exp_dir ^/ (M.bin_name ^ ".0.data_collector.v1.sexp")
      in
      Reader.load_sexp data_collector_file
        [%of_sexp: Data_collector.V1.Decision.t list]
      >>=? fun executed_decisions ->
      Experiment_utils.copy_compilation_artifacts
        ~exp_dir:M.exp_dir ~abs_path_to_binary:filename
        ~dump_dir:(
          Experiment_utils.Dump_utils.execution_dump_directory
            ~step:(`Step step) ~sub_id:(`Sub_id sub_id))
      >>=? fun () ->

      (* TODO: This is incredibly expensive -- there is a lot of potential
       * for tree structure sharing here.
       *)
      let tree = Inlining_tree.V1.build executed_decisions in
      let work_unit =
        { Work_unit. path_to_bin = filename; step = `Step step;
          sub_id = `Sub_id sub_id }
      in
      Deferred.Or_error.return { T1. tree; work_unit; }
    ;;

    let energy state =
      let work_unit = state.work_unit in
      Log.Global.sexp ~level:`Info
          [%message "dispatching" (work_unit : Experiment_utils.Work_unit.t)];
      Experiment_utils.Scheduler.dispatch M.scheduler state.work_unit
    ;;
  end

  include SA.Make(T2)

  let step t =
    let dump_directory =
      Experiment_utils.Dump_utils.execution_dump_directory
        ~step:(`Step t.step) ~sub_id:`Current
    in
    let%bind () = Async_shell.mkdir ~p:() dump_directory in
    let%bind () =
      Writer.save_sexp (dump_directory ^/ "state.sexp") ([%sexp_of: t] t)
    in
    let%bind (description, next) = step t in
    let%bind () =
      Writer.save_sexp (dump_directory ^/ "step.sexp")
        ([%sexp_of: Step.t] description)
    in
    let%bind () =
      Writer.save_sexp (dump_directory ^/ "initial_tree.sexp")
        ([%sexp_of: Inlining_tree.V1.Top_level.t] (fst description.initial).tree)
    in
    let%bind () =
      Writer.save_sexp (dump_directory ^/ "proposal_tree.sexp")
        ([%sexp_of: Inlining_tree.V1.Top_level.t] (fst description.proposal).tree)
    in
    return (description, next)
  ;;
end

let command_run =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"Command"
    [%map_open
      let {
        Command_params.
        config_filename;
        controller_rundir = _;
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
        Deferred.Or_error.return ()
        >>=? fun () ->
        Experiment_utils.copy_compilation_artifacts
          ~exp_dir ~abs_path_to_binary:initial_state.path_to_bin
          ~dump_dir:(
            Experiment_utils.Dump_utils.execution_dump_directory
              ~step:`Initial ~sub_id:`Current)
        >>=? fun () ->
        let process conn work_unit =
          let path_to_bin = work_unit.Work_unit.path_to_bin in
          let num_runs = config.num_runs in
          let dump_dir =
            Experiment_utils.Dump_utils.execution_dump_directory
              ~step:work_unit.step ~sub_id:work_unit.sub_id
          in
          lift_deferred (Async_shell.mkdir ~p:() dump_dir)
          >>=? fun () ->
          shell ~dir:exp_dir "ln" [
            "-s";
            "../current/artifacts.tar";
            dump_dir ^/ "artifacts.tar";
          ]
          >>=? fun () ->
          Experiment_utils.run_binary_on_worker
            ~processor:(Utils.Worker_connection.processor conn)
            ~num_runs ~conn ~path_to_bin
            ~hostname:(Utils.Worker_connection.hostname conn)
            ~bin_args
            ~dump_dir
          >>|? fun execution_stats ->
          let raw_execution_time = execution_stats.raw_execution_time in
          Log.Global.sexp ~level:`Info [%message
            (path_to_bin : string)
            (raw_execution_time : Time.Span.t list)];
          Log.Global.info "%s\n" execution_stats.gc_stats;
          execution_stats
        in
        lift_deferred (Utils.Scheduler.create worker_connections ~process)
        >>=? fun scheduler ->
        (* We run this 3 more times than the others to gurantee
         * stability of the distribution of initial execution times.
         *)
        Deferred.Or_error.List.init ~how:`Sequential 3 ~f:(fun i ->
          Log.Global.sexp ~level:`Info
            [%message "Initial state run " (i : int)];
          Deferred.Or_error.List.init (List.length config.worker_configs)
            ~how:`Parallel
            ~f:(fun j ->
              let path_to_bin = initial_state.path_to_bin in
              let work_unit =
                { Work_unit.
                  path_to_bin; step = `Initial;
                  sub_id = `Sub_id (i * (List.length config.worker_configs) + j);
                }
              in
              Utils.Scheduler.dispatch scheduler work_unit))
        >>=? fun stats ->
        let stats = List.concat_no_order stats in
        let initial_execution_times =
          List.concat_map stats ~f:(fun stat -> stat.raw_execution_time)
        in
        let initial_execution_stats =
          { Execution_stats.
            raw_execution_time = initial_execution_times;
            worker_hostname = None;
            gc_stats = (List.hd_exn stats).gc_stats;
            parsed_gc_stats = None;
            perf_stats = Some (List.concat_map stats ~f:(fun s ->
                Option.value ~default:[] s.Execution_stats.perf_stats));
          }
        in
        let initial_execution_time =
          List.map ~f:Time.Span.to_sec initial_execution_times
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
          let tree = Inlining_tree.V1.build initial_state.v1_decisions in
          let path_to_bin = initial_state.path_to_bin in
          let work_unit =
            { Work_unit. path_to_bin; step = `Step 0; sub_id = `Current; }
          in
          let initial = { Annealer.T1. tree; work_unit; } in
          Annealer.empty initial initial_execution_stats
        in
        Deferred.repeat_until_finished state (fun state ->
          Annealer.step state
          >>| fun ((_ : Annealer.Step.t), next) ->
          if next.step >= next.config.steps then
            `Finished (Ok ())
          else
            `Repeat next
        )
    ]

module Command_plot = struct
  let command_v0 =
    let open Command.Let_syntax in
    Command.async' ~summary:"Display"
      [%map_open
       let steps = flag "-steps" (required int) ~doc:"INT"
       and common_prefix = flag "-prefix" (required string) ~doc:"STRING"
       and output_file = flag "-output" (required file) ~doc:"PATH"
       in
       fun () ->
         let open Deferred.Let_syntax in
         let module Execution_stats = Protocol.Execution_stats in
         let module Base_state_energy = Base_state_energy.V0 in
         let module Step = Step.V0 in
         let files =
           List.init steps ~f:(fun i ->
             common_prefix ^/ Int.to_string i ^/ "current/step.sexp")
         in
         let%bind results =
           Deferred.List.map files ~how:(`Max_concurrent_jobs 32)
             ~f:(fun file ->
               Reader.load_sexp_exn file [%of_sexp: Step.t])
         in
         let get_initial (r : Step.t)
             : (Base_state_energy.state * Execution_stats.t) =
           r.initial
         in
         let get_proposal (r : Step.t)
             : (Base_state_energy.state * Execution_stats.t) =
           r.proposal
         in
         let execution_times
             ~(f : Step.t -> (Base_state_energy.state * Execution_stats.t)) =
           List.map results ~f:(fun result ->
               let result =
                 List.map ~f:Time.Span.to_sec
                   (snd (f result)).raw_execution_time
               in
               Fyp_stats.geometric_mean result)
           |> Array.of_list
         in
         let energy ~f ~base =
           let arr = execution_times ~f in
           Array.map arr ~f:(fun a -> a /. base)
         in
         let gc_stats
             ~(f : Step.t -> (Base_state_energy.state * Execution_stats.t)) =
           List.map results ~f:(fun result ->
             let result = snd (f result) in
             let gc_stats =
              match result.parsed_gc_stats with
              | None ->
                Option.value_exn (
                  Execution_stats.Gc_stats.parse (
                    String.split_lines result.gc_stats)
                )
              | Some x -> x
             in
             gc_stats)
           |> Array.of_list
         in
         let major_collections
             ~(f : Step.t -> (Base_state_energy.state * Execution_stats.t)) =
           Array.map (gc_stats ~f) ~f:(fun (gc : Execution_stats.Gc_stats.t) ->
             gc.major_collections)
         in
         let minor_collections
             ~(f : Step.t -> (Base_state_energy.state * Execution_stats.t)) =
           Array.map (gc_stats ~f) ~f:(fun (gc : Execution_stats.Gc_stats.t) ->
             gc.minor_collections)
         in
         let initial_execution_times = execution_times ~f:get_initial in
         let proposed_execution_times = execution_times ~f:get_proposal in
         let initial_energy = energy ~f:get_initial ~base:initial_execution_times.(0) in
         let proposed_energy = energy ~f:get_proposal ~base:initial_execution_times.(0) in
         let initial_major_collections = major_collections ~f:get_initial in
         let proposed_major_collections = major_collections ~f:get_proposal in
         let initial_minor_collections = minor_collections ~f:get_initial in
         let proposed_minor_collections = minor_collections ~f:get_proposal in

         assert (Array.length initial_execution_times = steps);
         assert (Array.length proposed_execution_times = steps);
         assert (Array.length initial_energy = steps);
         assert (Array.length proposed_energy = steps);
         assert (Array.length initial_major_collections = steps);
         assert (Array.length proposed_major_collections = steps);
         assert (Array.length initial_minor_collections = steps);
         assert (Array.length proposed_minor_collections = steps);

         let%bind output = Writer.open_file output_file in
         for step = 0 to steps - 1 do
           Writer.writef output "%d,%.3f,%.3f,%.3f,%.3f,%d,%d,%d,%d\n"
             step
             initial_execution_times.(step)
             proposed_execution_times.(step)
             initial_energy.(step)
             proposed_energy.(step)
             initial_major_collections.(step)
             proposed_major_collections.(step)
             initial_minor_collections.(step)
             proposed_minor_collections.(step);
         done;
         Writer.close output
      ]
  ;;

  let command_v1 =
    let open Command.Let_syntax in
    Command.async' ~summary:"Plots runtime vs various stats"
      [%map_open
       let common_prefix = anon ("directory" %: string) in
       fun () ->
         let open Deferred.Let_syntax in
         let module Execution_stats = Protocol.Execution_stats in
         let module Base_state_energy = Base_state_energy.V1 in
         let module Step = Step.V1 in
         let files =
           List.init 600 ~f:(fun step ->
             let cur =
               common_prefix
               ^/ "opt_data"
               ^/ Int.to_string step
               ^/ "current"
               ^/ "execution_stats.sexp"
             in
             let rest =
               (List.init 3 ~f:(fun worker_id ->
                 common_prefix
               ^/ "opt_data"
               ^/ Int.to_string step
               ^/ Int.to_string worker_id
               ^/ "execution_stats.sexp"))
             in
             cur :: rest)
           |> List.concat
         in
         let init_files =
           List.init 9 ~f:(fun worker_id ->
             common_prefix
             ^/ "opt_data"
             ^/ "initial"
             ^/ Int.to_string worker_id
             ^/ "execution_stats.sexp")
         in
         let files = init_files @ files in
         let load_instruction_count filename =
           Sys.file_exists_exn filename >>= function
           | true ->
             begin
             Reader.file_contents filename
             >>| fun text -> Some (Int.of_string (String.strip text))
             end
           | false -> return None
         in
         let%map execution_stats =
           Deferred.List.filter_map files ~how:(`Max_concurrent_jobs 32)
             ~f:(fun execution_stats_file ->
                Sys.file_exists_exn execution_stats_file >>= function
                | true ->
                  begin
                  Reader.load_sexp_exn execution_stats_file
                   [%of_sexp: Execution_stats.t]
                  >>= fun loaded ->
                  let instruction_count_file =
                    (Filename.dirname execution_stats_file) ^/ "instruction_count.txt"
                  in
                  load_instruction_count instruction_count_file
                  >>| fun instruction_count ->
                  Some (execution_stats_file, loaded, instruction_count)
                  end
                | false -> return None)
         in
         let first_perf_fields = [
           "branches";
           "branch-misses";
           "branches";
           "branch-misses";
           "L1-icache-load-misses";
           "branch-load-misses";
           "branch-loads";
           "iTLB-load-misses";
           "iTLB-loads";
           "cpu-cycles";
           "instructions";
         ]
         in
         let second_perf_fields = [
           "branches";
           "branch-misses";
           "L1-icache-load-misses";
           "branch-load-misses";
           "branch-loads";
           "iTLB-load-misses";
           "iTLB-loads";
           "cpu-cycles";
           "instructions";
           "L1-dcache-load-misses";
           "L1-dcache-loads";
           "L1-dcache-stores";
           "LLC-loads";
           "LLC-stores";
           "cache-misses";
           "bus-cycles";
           "cache-references";
           "ref-cycles";
           "dTLB-load-misses";
           "dTLB-loads";
           "dTLB-store-misses";
           "dTLB-stores";
         ]
         in
         List.iter execution_stats ~f:(fun (file, stat, instruction_count) ->
           let exec_time = geometric_mean (
             List.map ~f:Time.Span.to_sec stat.raw_execution_time)
           in
           let hostname = stat.worker_hostname in
           let gc_stats =
             match stat.parsed_gc_stats with
             | None ->
               Option.value_exn (
                 Execution_stats.Gc_stats.parse
                  (String.split_lines stat.gc_stats))
             | Some x -> x
           in
           printf "%s," file;
           printf "%s," (Option.value_exn hostname);
           printf "%d," gc_stats.major_collections;
           printf "%d," gc_stats.minor_collections;
           printf "%d," gc_stats.compactions;
           printf "%d," gc_stats.minor_words;
           printf "%d," gc_stats.promoted_words;
           printf "%d," gc_stats.major_words;
           printf "%d," gc_stats.top_heap_words;
           printf "%d," gc_stats.heap_words;
           printf "%d," gc_stats.live_words;
           printf "%d," gc_stats.free_words;
           printf "%d," gc_stats.largest_free;
           printf "%d," gc_stats.fragments;
           printf "%d," gc_stats.live_blocks;
           printf "%d," gc_stats.heap_chunks;

           begin match Option.value_exn stat.perf_stats with
           | [ first_perf_stats; second_perf_stats ] ->
             let first_perf_stats = Perf_stats.to_map first_perf_stats in
             let second_perf_stats = Perf_stats.to_map second_perf_stats in
             List.iter first_perf_fields ~f:(fun name ->
               printf "%d," (String.Map.find_exn first_perf_stats name).value);
             List.iter second_perf_fields ~f:(fun name ->
               printf "%d," (
                 match String.Map.find second_perf_stats name with
                 | None -> failwithf "cannot find: %s" name ()
                 | Some x -> x
               ).value);
           | _ -> assert false
           end;
           begin match instruction_count with
           | Some x -> printf "%d," x
           | None -> printf "N/A,"
           end;
           printf "%.4f\n" exec_time)
      ]
  ;;

  let command =
    Command.group ~summary:"Something"
      [("v0", command_v0);
       ("v1-execution-stats", command_v1);
      ]
  ;;
end

let command =
  Command.group ~summary:"Simulated Annealing"
    [("plot", Command_plot.command);
     ("run",  command_run);
    ]
