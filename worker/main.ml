open Core
open Core_extended
open Async

module Relpath = Protocol.Relpath
module Config = Protocol.Config
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc
module Info_rpc = Protocol.Info_rpc

let shell ?(env = []) ?(echo = false) ?(verbose = false) ~dir:working_dir
    prog args =
  Monitor.try_with_or_error (fun () ->
      let env = `Extend env in
      Async_shell.run ~echo ~verbose ~working_dir ~env prog args)
;;

let time_stderr_regex = Re2.Regex.create_exn "^(\\d+\\.\\d+)\\s*user"

let aggregrate_benchmarks (benchmarks : Protocol.Benchmark_results.t list) =
  let n = List.length benchmarks in
  assert (n > 0);
  let execution_time =
    benchmarks
    |> List.map ~f:(fun b -> b.execution_time)
    |> List.sum (module Time.Span) ~f:Fn.id
    |> (fun a -> Time.Span.(a / float_of_int n))
  in
  { Protocol.Benchmark_results. execution_time }
;;

let compile_and_run_benchmark ~num_runs ~rundir
    ~(compile_params : Protocol.Compile_params.t option)
    ~(compiler_selection : Protocol.Compiler_selection.t)
    ~(benchmark : Protocol.Benchmark.t) =
  let working_dir = rundir ^/ Relpath.to_string benchmark.dir in
  let path_to_compiler =
    match compiler_selection with
    | Flambda -> rundir ^/ "ocaml-flambda/_install/bin/ocamlopt.opt"
    | Ours -> failwith "[Ours] compiler not implemented"
  in
  let compiler_flags =
    match compile_params with
    | None -> ""
    | Some params ->
      sprintf
        "-inline %.3f -inline-toplevel %d -inline-alloc-cost %d \
         -inline-branch-cost %d -inline-call-cost %d -inline-prim-cost %d \
         -inline-indirect-cost %d -inline-lifting-benefit %d"
        params.inline
        params.inline_top_level
        params.inline_alloc_cost
        params.inline_branch_cost
        params.inline_call_cost
        params.inline_prim_cost
        params.inline_indirect_cost
        params.inline_lifting_benefit
  in
  let env =
    [("OCAMLOPT", sprintf "%s %s" path_to_compiler compiler_flags)]
  in
  let shell = shell ~env ~dir:working_dir in
  Log.Global.sexp ~level:`Info [%message (working_dir : string)];
  shell ~verbose:true ~echo:true "make" [ "clean" ]
  >>=? fun () -> shell "make" ~verbose:true ~echo:true [ benchmark.executable ]
  >>=? fun () ->
  Log.Global.sexp ~level:`Info  [%message "Running benchmarks now"];
  Deferred.Or_error.List.init num_runs (fun _ ->
      Monitor.try_with_or_error (fun () ->
          let args =
            (working_dir ^/ benchmark.executable) :: benchmark.run_args
          in
          let%map (_stdout, stderr) =
            Async_shell.run_full_and_error "/usr/bin/time" args
          in
          (* Exeception here is fine, because we are running in a monitor *)
          let execution_time =
            Re2.Regex.first_match_exn time_stderr_regex stderr
            |> Re2.Regex.Match.get_exn ~sub:(`Index 1)
            |> float_of_string
            |> Time.Span.of_sec
          in
          { Protocol.Benchmark_results. execution_time }))
;;

let job_dispatcher_impl ~rundir ~config () query =
  Log.Global.sexp ~level:`Info
    [%message "Received query from " (query : Job_dispatch_rpc.Query.t)];
  let num_runs = config.Config.num_runs in
  begin match query with
  | Job_dispatch_rpc.Query.Run_binary relpath ->
    let executable = rundir ^/ Relpath.to_string relpath in
    Deferred.Or_error.List.init num_runs (fun _ ->
        Monitor.try_with_or_error (fun () ->
            let args = [ executable ] in
            let%map (_stdout, stderr) =
              Async_shell.run_full_and_error "/usr/bin/time" args
            in
            (* Exeception here is fine, because we are running in a monitor *)
            let execution_time =
              Re2.Regex.first_match_exn time_stderr_regex stderr
              |> Re2.Regex.Match.get_exn ~sub:(`Index 1)
              |> float_of_string
              |> Time.Span.of_sec
            in
            { Protocol.Benchmark_results. execution_time }))
    >>|? aggregrate_benchmarks
  end
  >>= function
  | Ok bench_result ->
    return (Job_dispatch_rpc.Response.Success bench_result)
  | Error (error : Error.t) ->
    Log.Global.sexp ~level:`Info [%message (error : Error.t)];
    return (Job_dispatch_rpc.Response.Failed error)
;;

let information_rpc_impl
    ~(config : Config.t) ~rundir () (query : Info_rpc.Query.t) =
  match query with
  | Where_to_copy ->
    let relpath = config.worker_direct_exec_dir in
    Deferred.return { Info_rpc.Response. rundir; relpath; }

let start_server ~config ~rundir ~stop =
  let port = config.Config.worker_port in
  let implementations =
    [ Rpc.Rpc.implement Job_dispatch_rpc.rpc
        (job_dispatcher_impl ~config ~rundir);
      Rpc.Rpc.implement Info_rpc.rpc
        (information_rpc_impl ~config ~rundir);
    ]
  in
  let implementations =
    Rpc.Implementations.create_exn ~implementations
      ~on_unknown_rpc:`Close_connection
  in
  let%bind server =
    Tcp.Server.create ~on_handler_error:`Ignore
      (Tcp.on_port port)
      (fun _addr r w ->
         Rpc.Connection.server_with_close r w
           ~connection_state:(fun _ -> ())
           ~on_handshake_error:`Ignore
           ~implementations)
  in
  Log.Global.sexp ~level:`Info [%message
     "Ready to accept connections at " (port : int)];
  Deferred.any
    [ (stop >>= fun () -> Tcp.Server.close server);
      (Tcp.Server.close_finished server);
    ]
;;

let () =
  let open Command.Let_syntax in
  Command.async' ~summary:"Hello"
    [%map_open
      let config_filename =
        flag "-filename" (required file) ~doc:"PATH to config file"
      and rundir = flag "-rundir" (required string) ~doc:"rundir" in
      let open Deferred.Let_syntax in
      fun () ->
        let%bind config =
          Reader.load_sexp_exn config_filename [%of_sexp: Config.t]
        in
        start_server ~rundir ~config ~stop:(Deferred.never ())
    ]
  |> Command.run
;;
