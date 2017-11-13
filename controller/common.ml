[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

module Config = Protocol.Config
module Info_rpc = Protocol.Info_rpc
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc
module Relpath = Protocol.Relpath
module Execution_stats = Protocol.Execution_stats
module Results = Protocol.Results
module Work_unit_id = Results.Work_unit_id

let rec comp_sexp_of_core_sexp core_sexp =
  match core_sexp with
  | Core.Sexp.Atom s -> Fyp_compiler_lib.Sexp.Atom s
  | Core.Sexp.List l ->
    Fyp_compiler_lib.Sexp.List (List.map ~f:comp_sexp_of_core_sexp l)
;;

let rec core_sexp_of_comp_sexp comp_sexp =
  match comp_sexp with
  | Fyp_compiler_lib.Sexp.Atom s -> Core.Sexp.Atom s
  | Fyp_compiler_lib.Sexp.List l ->
    Core.Sexp.List (List.map ~f:core_sexp_of_comp_sexp l)
;;

module Make_core_sexp(M :
  sig type t

  val sexp_of_t : t -> Fyp_compiler_lib.Sexp.t
  val t_of_sexp : Fyp_compiler_lib.Sexp.t -> t
end) = struct
  let sexp_of_t t = core_sexp_of_comp_sexp (M.sexp_of_t t)
  let t_of_sexp sexp = M.t_of_sexp (comp_sexp_of_core_sexp sexp)
end

module Data_collector = struct
  include Fyp_compiler_lib.Data_collector
  include Make_core_sexp(Fyp_compiler_lib.Data_collector)
end

module Closure_id = struct
  include Fyp_compiler_lib.Closure_id
  include Make_core_sexp(Fyp_compiler_lib.Closure_id)
end

module Call_site = struct
  include Fyp_compiler_lib.Call_site
  include Make_core_sexp(Fyp_compiler_lib.Call_site)

end

module Call_site_offset = struct
  include Call_site.Offset
  include Make_core_sexp(Call_site.Offset)
end

let shell ?(env = []) ?(echo = false) ?(verbose = false) ~dir:working_dir
    prog args =
  Monitor.try_with_or_error (fun () ->
      let env = `Extend env in
      Async_shell.run ~echo ~verbose ~working_dir ~env prog args)
;;

let rec is_prefix
    ~(prefix : Call_site.t list) (test : Call_site.t list) =
  match prefix, test with
  | [], [] -> false
  | [], _ -> true
  | _, [] -> false
  | prefix_hd :: prefix_tl, hd :: tl ->
    Call_site.equal prefix_hd hd &&
    is_prefix ~prefix:prefix_tl tl

let filter_decisions (decisions : Data_collector.t list) =
  List.filter decisions ~f:(fun test ->
    not (
      List.exists decisions ~f:(fun decision ->
        if phys_equal test decision
        then false
        else (
          not decision.decision &&
          is_prefix ~prefix:(List.rev decision.call_stack) (List.rev test.call_stack)
        )
      )
    )
  )

let lift_deferred m = Deferred.(m >>| fun x -> Core.Or_error.return x)

module Worker_connection = struct
  type 'a rpc_conn =
    { socket : ([`Active], 'a) Socket.t;
      reader : Reader.t;
      writer : Writer.t;
      rpc_connection : Rpc.Connection.t
    }

  type ssh_conn = { hostname: string; rundir: string; }

  type 'a t =
    | Rpc : 'a rpc_conn -> 'a t
    | Ssh : ssh_conn -> 'a t
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

let run_binary_on_ssh_worker ~config ~rundir ~hostname ~path_to_bin ~bin_args =
  lift_deferred (Unix.getcwd ())
  >>=? fun dir ->
  shell ~echo:true ~dir "scp"
    [ path_to_bin;
      hostname ^ ":" ^ rundir ^/ "binary.exe";
    ]
  >>=? fun () ->
  Async_shell.run_lines ~echo:true ~working_dir:dir "ssh" [
    hostname;
    rundir ^/ "benchmark_binary.sh";
    rundir ^/ "binary.exe";
    Int.to_string (config.Config.num_runs);
    bin_args;
  ]
  >>= function
  | [] -> Deferred.Or_error.error_s [%message "Something went wrong"]
  | lines ->
    let raw_execution_time =
      List.map lines ~f:(fun line ->
          Time.Span.of_sec (Float.of_string line))
    in
    let worker_hostname = Some hostname in
    Deferred.Or_error.return (
      { Execution_stats. raw_execution_time; worker_hostname; }
    )
;;

let run_binary_on_worker ~config ~hostname ~conn ~path_to_bin ~bin_args =
  match conn with
  | Worker_connection.Rpc worker_connection ->
    run_binary_on_rpc_worker ~worker_connection ~path_to_bin ~hostname
  | Worker_connection.Ssh (ssh_config : Worker_connection.ssh_conn) ->
    let rundir = ssh_config.rundir in
    run_binary_on_ssh_worker ~config ~rundir ~hostname ~path_to_bin ~bin_args
;;

let init_connection ~hostname ~worker_config =
  match worker_config with
  | Protocol.Config.Ssh_worker ssh_config ->
    begin
    lift_deferred (Unix.getcwd ())
    >>=? fun cwd ->
    let args =
      [ "worker/benchmark_binary.sh"; (hostname ^ ":" ^ ssh_config.rundir) ]
    in
    shell ~dir:cwd "scp" args >>|? fun () ->
    let rundir = ssh_config.rundir in
    Worker_connection.Ssh { rundir; hostname; }
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
        { Worker_connection. socket; reader; writer; rpc_connection; }
      in
      Ok (Worker_connection.Rpc rpc_connection)
    | Error exn -> raise exn
