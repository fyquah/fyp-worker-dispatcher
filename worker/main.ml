open Core
open Async

module Config = Protocol.Config
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc

let job_dispatcher_impl () query =
  Log.Global.sexp ~level:`Info
    [%message "Received query from " (query : Job_dispatch_rpc.Query.t)];
  let%bind () = Clock.after (sec 60.0) in
  let execution_time = Time.Span.of_sec 60.0 in
  return { Job_dispatch_rpc.Response. execution_time }
;;

let start_server ~port ~stop =
  let implementations =
    [ Rpc.Rpc.implement Job_dispatch_rpc.rpc job_dispatcher_impl ]
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
      in
      let open Deferred.Let_syntax in
      fun () ->
        let%bind config =
          Reader.load_sexp_exn config_filename [%of_sexp: Config.t]
        in
        start_server ~port:config.worker_port ~stop:(Deferred.never ())
    ]
  |> Command.run
;;
