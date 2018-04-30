[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common

let rec build_sliding_window ~n input_list =
  if List.length input_list <= n then
    []
  else
    (List.take input_list n)
    :: build_sliding_window ~n (List.tl_exn input_list)
;;

type work_unit =
  { overrides   : Data_collector.V0.t list;
    decisions   : Data_collector.V0.t list;
    path_to_bin : string;
    id          : Results.Work_unit_id.t;
  }

(* - [base_overrides] refers to overrides that should be kept throughout
 *   every compilation -- the sliding window does not include base overrides.
 * - [new_overrides] refers to new overrides to run the sliding window over.
 *   We want to select good overrides here.
 *
 * This funciton assumes that [base_overrides] and [new_overrides] are
 * non-conflicting and mutually exclusive sets of decisions.
 *)
let slide_over_decisions
    ~config ~worker_connections ~bin_name ~exp_dir ~results_dir
    ~base_overrides ~new_overrides ~bin_args =
  let sliding_window = build_sliding_window ~n:2 new_overrides in
  let hostnames =
    List.map ~f:Protocol.Config.hostname config.Config.worker_configs
  in
  let latest_work = Mvar.create () in
  Deferred.don't_wait_for (
    Deferred.Or_error.List.iter ~how:`Sequential sliding_window ~f:(fun decisions ->
        let flipped_decisions =
          List.map decisions ~f:(fun (d : Data_collector.V0.t) ->
              { d with decision = not d.decision })
        in
        shell ~echo:true ~verbose:true ~dir:exp_dir "make" [ "clean" ]
        >>=? fun () ->
        lift_deferred (
          Writer.save_sexp (exp_dir ^/ "overrides.sexp")
            ([%sexp_of: Data_collector.V0.t list]
              (base_overrides @ flipped_decisions))
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
        Reader.load_sexp (exp_dir ^/ (bin_name ^ ".0.data_collector.v0.sexp"))
          [%of_sexp: Data_collector.V0.t list]
        >>=? fun executed_decisions ->
        let path_to_bin = filename in
        let overrides = flipped_decisions in
        let decisions = executed_decisions in
        let id = Results.Work_unit_id.gen () in
        lift_deferred (
          Mvar.put latest_work (
            Some { path_to_bin; overrides; decisions; id; }
          )
        )
    )
    >>= function
    | Ok () -> Mvar.put latest_work None
    | Error e -> failwithf !"Error %{sexp#hum: Error.t}" e ()
  );
  let results_acc = ref [] in
  Deferred.Or_error.List.iter
    (List.zip_exn worker_connections hostnames)
    ~how:`Parallel ~f:(fun (conn, hostname) ->
      Deferred.repeat_until_finished () (fun () ->
        Mvar.take latest_work
        >>= function
        | None ->
          Mvar.set latest_work None;
          Deferred.return (`Finished (Ok ()))
        | Some work_unit ->
          begin
          let path_to_bin = work_unit.path_to_bin in
          printf "Running %s\n" path_to_bin;
          Experiment_utils.run_binary_on_worker
            ~dump_dir:results_dir
            ~processor:(Experiment_utils.Worker_connection.processor conn)
            ~num_runs:config.num_runs
            ~conn ~path_to_bin ~hostname ~bin_args
          >>= function
          | Ok benchmark ->
            printf "Done with %s. Saving results ...\n" path_to_bin;
            Unix.mkdir ~p:() results_dir
            >>= fun () ->
            let work_unit_id = work_unit.id in
            let results =
              let id = work_unit.id in
              let overrides = work_unit.overrides in
              let decisions = work_unit.decisions in
              let path_to_bin = Some path_to_bin in
              { Results. id; benchmark; overrides; decisions; path_to_bin; }
            in
            results_acc := results :: !results_acc;
            let filename =
              results_dir ^/
              (sprintf !"results_%{Results.Work_unit_id}.sexp"
                work_unit_id)
            in
            Writer.save_sexp filename (Results.sexp_of_t results)
            >>| fun () -> `Repeat ()
          | Error e -> Deferred.return (`Finished (Error e))
          end))
    >>|? fun () -> !results_acc
;;

let command =
  let module Generation = struct
    type t =
      { base_overrides : Data_collector.V0.t list;
        gen            : int;
      }
  end
  in
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"Controller"
    [%map_open
      let {
        Command_params.
        config_filename;
        controller_rundir;
        exp_dir;
        bin_name;
        bin_args } = Command_params.params in
     fun () ->
       (* There is daylight saving now, so UTC timezone == G time zone :D *)
       Reader.load_sexp config_filename [%of_sexp: Config.t]
       >>=? fun config ->
       Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
         ~f:(fun worker_config ->
           let hostname = Protocol.Config.hostname worker_config in
           Experiment_utils.init_connection ~hostname ~worker_config)
       >>=? fun worker_connections ->

       Deferred.repeat_until_finished { Generation. base_overrides = []; gen = 0 }
         (fun generation ->
           let print_sexp sexp =
             List.iter
               (sexp |> Sexp.to_string_hum |> String.split_lines)
               ~f:(fun line -> printf "[GENERATION %d] %s\n" generation.gen line);
           in
           Log.Global.info ">>>= Running generation %d" generation.gen;
           printf "[GENERATION %d] Starting generation!\n" generation.gen;
           printf "[GENERATION %d] Base overrides:\n" generation.gen;
           print_sexp ([%sexp_of: Data_collector.V0.t list] generation.base_overrides);
           begin
             let base_overrides = generation.base_overrides in
             Experiment_utils.get_initial_state
               ~bin_name ~exp_dir ~base_overrides ()
             >>=? fun state ->
             let { Experiment_utils.Initial_state.
               v0_decisions = new_overrides;
               path_to_bin = _;
               v1_decisions = _;
             } = Option.value_exn state in
             let new_overrides =
               (* This filter ensures that every the base and overrides
                * sets are mutually exclusive.
                *
                * Base is definitely non conflicting, as it is generated
                * from the previous generation whereas new_overrides is
                * non conflicting due to the filter_decisions call earlier.
                *)
               List.filter new_overrides ~f:(fun override ->
                 not (
                   List.exists generation.base_overrides ~f:(fun base ->
                     Data_collector.V0.equal base override)
                   )
                 )
             in
             let results_dir =
               controller_rundir
               ^/ Int.to_string_hum generation.gen
             in
             printf "[GENERATION %d] Generation choices:\n" generation.gen;
             print_sexp ([%sexp_of: Data_collector.V0.t list] new_overrides);

             slide_over_decisions ~bin_name ~config ~worker_connections
               ~base_overrides:generation.base_overrides
               ~exp_dir ~results_dir ~new_overrides ~bin_args
           end
           >>| function
           | Ok results ->
             printf "[GENERATION %d] Results summary:\n" generation.gen;
             List.iter results ~f:(fun result ->
               let seconds =
                 Array.of_list_map result.benchmark.raw_execution_time
                   ~f:Time.Span.to_sec
               in
               let mean = Time.Span.of_sec (arithmetic_mean (Array.to_list seconds)) in
               let sd = Time.Span.of_sec (standard_deviation (Array.to_list seconds)) in
               printf
                 !"[GENERATION %d] Work unit %{Work_unit_id}: %{Time.Span} (sd: %{Time.Span})\n"
                 generation.gen
                 result.id
                 mean
                 sd
             );
             let selected_results = Selection_and_mutate.selection results in
             let old_base = generation.base_overrides in
             begin match
               Selection_and_mutate.mutate ~old_base selected_results
             with
             | None ->
               printf "[GENERATION %d] Terminating at generation %d\n"
                 generation.gen generation.gen;
               `Finished (Ok ())
             | Some new_base ->
               printf
                 "[GENERATION %d] Selected work unit ids for mutation = %s\n"
                 generation.gen
                 (String.concat ~sep:", "
                   (List.map selected_results ~f:(fun result ->
                     (Results.Work_unit_id.to_string result.Results.id))));
               `Repeat (
                 { Generation. gen = generation.gen + 1;
                   base_overrides = new_base;
                 }
               )
             end
           | Error e -> `Finished (Error e))
       ]
