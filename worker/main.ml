open Core
open Core_extended
open Async

module Relpath = Protocol.Relpath
module Config = Protocol.Config
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc

let shell ?(env = []) ~dir:working_dir prog args =
  Monitor.try_with_or_error (fun () ->
      let env = `Extend env in
      Async_shell.run ~working_dir ~env prog args)
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

let compile_and_run_benchmark ~num_runs ~working_dir
    ~(benchmark : Protocol.Benchmark.t) =
  let shell = shell ~dir:working_dir in
  Log.Global.sexp ~level:`Info [%message (working_dir : string)];
  shell "make" [ "clean" ]
  >>=? fun () -> shell "make" [ benchmark.executable ]
  >>=? fun () ->
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
  >>|? aggregrate_benchmarks
;;

let job_dispatcher_impl ~rundir ~config () query =
  Log.Global.sexp ~level:`Info
    [%message "Received query from " (query : Job_dispatch_rpc.Query.t)];
  let benchmark = query.targets in
  let working_dir = rundir ^/ Relpath.to_string benchmark.dir in
  let num_runs = config.Config.num_runs in
  match%bind compile_and_run_benchmark ~num_runs ~working_dir ~benchmark with
  | Ok bench_result ->
    return (Job_dispatch_rpc.Response.Success bench_result)
  | Error (error : Error.t) ->
    Log.Global.sexp ~level:`Info [%message (error : Error.t)];
    return (Job_dispatch_rpc.Response.Failed error)
;;

let start_server ~config ~rundir ~stop =
  let port = config.Config.worker_port in
  let implementations =
    [ Rpc.Rpc.implement Job_dispatch_rpc.rpc
        (job_dispatcher_impl ~config ~rundir) ]
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
