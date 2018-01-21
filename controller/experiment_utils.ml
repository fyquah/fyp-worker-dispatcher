[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common


module Work_unit = struct
  type t =
    { path_to_bin : string;
      step : int;
      sub_id : int;
    }
  [@@deriving sexp]
end


module Initial_state = struct
  type t =
    { decisions : Data_collector.t list;
      traversal_state : Traversal_state.t;
      path_to_bin  : string;
    }
end

let get_initial_state ?(env = []) ~bin_name ~exp_dir ~base_overrides () =
  shell ~dir:exp_dir "make" [ "clean" ] >>=? fun () ->
  lift_deferred (
    Writer.save_sexp (exp_dir ^/ "overrides.sexp")
      ([%sexp_of: Data_collector.t list] base_overrides)
  )
  >>=? fun () ->
  shell ~env:env ~dir:exp_dir "make" [ "all" ] >>=? fun () ->
  let filename = exp_dir ^/ (bin_name ^ ".0.data_collector.sexp") in
  Reader.load_sexp filename [%of_sexp: Data_collector.t list]
  >>=? fun decisions ->
  let filename = Filename.temp_file "fyp-" ("-" ^ bin_name) in
  shell ~dir:exp_dir
    "cp" [ (bin_name ^ ".native"); filename ]
  >>=? fun () ->
  shell ~dir:exp_dir "chmod" [ "755"; filename ]
  >>|? fun () ->
  match decisions with
  | _ :: _ ->
    let decisions = filter_decisions decisions in
    Some {
      Initial_state.
      decisions;
      traversal_state = Traversal_state.init (Inlining_tree.build decisions);
      path_to_bin = filename;
    }
  | [] -> None
;;

module Worker_connection = struct
  type 'a rpc_conn =
    { hostname : string;
      socket : ([`Active], 'a) Socket.t;
      reader : Reader.t;
      writer : Writer.t;
      rpc_connection : Rpc.Connection.t
    }

  type ssh_conn = { user: string; hostname: string; rundir: string; }

  type 'a t =
    | Rpc : 'a rpc_conn -> 'a t
    | Ssh : ssh_conn -> 'a t

  let hostname t =
    match t with
    | Rpc c -> c.hostname
    | Ssh c -> c.hostname
end

let run_binary_on_rpc_worker ~hostname ~worker_connection ~path_to_bin =
  let conn = worker_connection.Worker_connection.rpc_connection in
  Rpc.Rpc.dispatch Info_rpc.rpc conn Info_rpc.Query.Where_to_copy
  >>=? fun path_to_rundir ->
  (* TODO: remove hard code *)
  let abs_path_to_rundir =
    path_to_rundir.Info_rpc.Response.rundir
    ^/ Relpath.to_string (path_to_rundir.Info_rpc.Response.relpath)
  in
  let args = [ path_to_bin; sprintf "%s:%s" hostname abs_path_to_rundir ] in
  shell ~dir:(Filename.dirname path_to_bin) "scp" args
  >>=? fun () ->
  let query =
    Job_dispatch_rpc.Query.Run_binary (
      Relpath.of_string (
        Relpath.to_string path_to_rundir.relpath
        ^/ Filename.basename path_to_bin
      )
    )
  in
  Rpc.Rpc.dispatch Job_dispatch_rpc.rpc conn query
  >>=? fun response ->
  match response with
  | Success results -> Deferred.Or_error.return results
  | Failed e -> Deferred.return (Error e)
;;

let run_binary_on_ssh_worker
    ~num_runs ~config:_ ~rundir ~user ~hostname ~path_to_bin ~bin_args =
  lift_deferred (Unix.getcwd ())
  >>=? fun dir ->
  shell ~dir "scp"
    [ path_to_bin;
      sprintf "%s@%s:%s" user hostname (rundir ^/ "binary.exe");
    ]
  >>=? fun () ->
  Async_shell.run_lines ~working_dir:dir "ssh" ([
    sprintf "%s@%s" user hostname;
    rundir ^/ "benchmark_binary.sh";
    rundir ^/ "binary.exe";
    Int.to_string num_runs;
    bin_args;
  ])
  >>= function
  | [] -> Deferred.Or_error.error_s [%message "Something went wrong"]
  | lines ->
    let raw_execution_time =
      List.map lines ~f:(fun line ->
          Time.Span.of_sec (Float.of_string line))
    in
    let worker_hostname = Some hostname in
    Async_shell.run_full ~working_dir:dir "ssh" [
      sprintf "%s@%s" user hostname;
      rundir ^/ "get_gc_stats.sh";
      rundir ^/ "binary.exe";
      bin_args;
    ]
    >>| fun gc_stats ->
    Ok { Execution_stats. raw_execution_time; worker_hostname; gc_stats;
         parsed_gc_stats = None}
;;

let run_binary_on_worker ~num_runs ~config ~hostname ~conn ~path_to_bin ~bin_args =
  match conn with
  | Worker_connection.Rpc worker_connection ->
    run_binary_on_rpc_worker ~worker_connection ~path_to_bin ~hostname
  | Worker_connection.Ssh (ssh_config : Worker_connection.ssh_conn) ->
    let rundir = ssh_config.rundir in
    let user = ssh_config.user in
    run_binary_on_ssh_worker ~num_runs ~user ~config ~rundir ~hostname ~path_to_bin ~bin_args
;;

let init_connection ~hostname ~worker_config =
  match worker_config with
  | Protocol.Config.Ssh_worker ssh_config ->
    begin
    lift_deferred (Unix.getcwd ())
    >>=? fun cwd ->
    let user = ssh_config.user in
    let args =
      [ "worker/benchmark_binary.sh";
        (sprintf "%s@%s:%s" user hostname ssh_config.rundir);
      ]
    in
    shell ~dir:cwd "scp" args >>=? fun () ->
    let args =
      [ "worker/get_gc_stats.sh";
        (sprintf "%s@%s:%s" user hostname ssh_config.rundir);
      ]
    in
    shell ~dir:cwd "scp" args >>|? fun () ->
    let rundir = ssh_config.rundir in
    let user = ssh_config.user in
    Worker_connection.Ssh { user; rundir; hostname; }
    end
  | Protocol.Config.Rpc_worker rpc_config ->
    lift_deferred (
      Tcp.connect
        (Tcp.to_host_and_port hostname rpc_config.port)
        ~timeout:(Time.Span.of_int_sec 1)
    )
    >>=? fun (socket, reader, writer) ->
    Rpc.Connection.create reader writer ~connection_state:(fun _ -> ())
    >>| function
    | Ok rpc_connection ->
      let rpc_connection =
        { Worker_connection.
          hostname; socket; reader; writer; rpc_connection; }
      in
      Ok (Worker_connection.Rpc rpc_connection)
    | Error exn -> raise exn


module Scheduler = struct
  type 'a availble_workers =
    | Available of 'a Worker_connection.t * 'a Worker_connection.t list
    | Completed

  type ('a, 'b, 'c) t =
    { queue       :
        ('c availble_workers,
         [ `Read | `Who_can_write of Core_kernel__Perms.me ]) Mvar.t;
      process     : ('c Worker_connection.t -> 'a -> 'b Deferred.t);
      num_workers : int;
    }

  let num_workers t = t.num_workers

  let add_conn_to_queue q conn =
    match q with
    | None -> Available (conn, [])
    | Some (Available (hd, tl)) -> Available (conn, hd :: tl)
    | Some Completed -> Completed
  ;;

  let create connections ~process =
    let worker_queue = Mvar.create () in
    Deferred.List.iter connections
      ~how:`Parallel ~f:(fun conn ->
        Mvar.update worker_queue ~f:(fun q -> add_conn_to_queue q conn) ;
        Deferred.unit)
    >>| fun () ->
    { queue = worker_queue; process; num_workers = List.length connections; }
  ;;

  let dispatch t a =
    let%bind avail = Mvar.take t.queue in
    begin match avail with
    | Completed -> failwith "Dispatching working to completed scheduler"
    | Available (conn, left) ->
      match left with
      | [] -> Deferred.return conn
      | hd :: tl -> Mvar.put t.queue (Available (hd, tl)) >>| fun () -> conn
    end
    >>= fun conn -> t.process conn a
    >>| fun result ->
    Mvar.update t.queue ~f:(fun q -> add_conn_to_queue q conn);
    result
  ;;

  let terminate t = Mvar.set t.queue Completed
end

let process_work_unit
    ~(config: Config.t) ~(bin_args: string)
    (conn: 'a Worker_connection.t) (work_unit: Work_unit.t) =
  let path_to_bin = work_unit.Work_unit.path_to_bin in
  let num_runs =
    config.num_runs / List.length config.worker_configs
  in
  run_binary_on_worker
    ~num_runs ~config ~conn ~path_to_bin
    ~hostname:(Worker_connection.hostname conn)
    ~bin_args
  >>|? fun execution_stats ->
  let raw_execution_time = execution_stats.raw_execution_time in
  Log.Global.sexp ~level:`Info [%message
    (path_to_bin : string)
    (raw_execution_time : Time.Span.t list)];
  Log.Global.info "%s\n" execution_stats.gc_stats;
  execution_stats
;;

let compile_binary ~dir ~bin_name overrides =
  shell ~dir "make" [ "clean" ]
  >>=? fun () ->
  lift_deferred (
    Writer.save_sexp (dir ^/ "overrides.sexp")
      ([%sexp_of: Data_collector.t list] overrides)
  )
  >>=? fun () ->
  shell ~dir "make" [ "all" ]
  >>=? fun () ->
  let filename = Filename.temp_file "fyp-" ("-" ^ bin_name) in
  shell ~dir "cp" [ (bin_name ^ ".native"); filename ]
  >>=? fun () ->
  shell ~dir "chmod" [ "755"; filename ]
  >>=? fun () ->
  Deferred.Or_error.return filename
;;

(** Useful to get a stable estimate of the baseline runtime **)
let run_in_all_workers ~scheduler ~times ~config ~path_to_bin =
  (* We run this 3 more times than the others to gurantee
   * stability of the distribution of initial execution times.
   *)
  Deferred.Or_error.List.init ~how:`Sequential times ~f:(fun i ->
    Log.Global.sexp ~level:`Info
      [%message "Initial state run " (i : int)];
    Deferred.Or_error.List.init (List.length config.Config.worker_configs)
      ~how:`Parallel
      ~f:(fun _ ->
        let work_unit =
          { Work_unit. path_to_bin; step = -1; sub_id = 0; }
        in
        Scheduler.dispatch scheduler work_unit))
  >>=? fun stats ->
  let stats = List.concat_no_order stats in
  let initial_execution_times =
    List.concat_map stats ~f:(fun (stat : Execution_stats.t) -> stat.raw_execution_time)
  in
  let execution_stats =
    { Execution_stats.
      raw_execution_time = initial_execution_times;
      worker_hostname = None;
      gc_stats = (List.hd_exn stats).gc_stats;
      parsed_gc_stats = None;
    }
  in
  Deferred.Or_error.return execution_stats
