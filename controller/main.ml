open Core
open Async

module Config = Protocol.Config
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc

let do_something ~conn =
  let query =
    { Job_dispatch_rpc.Query.
      compile_params = None;
    }
  in
  let%map response =
    Rpc.Rpc.dispatch_exn Job_dispatch_rpc.rpc conn query
  in
  Log.Global.sexp ~level:`Info
    [%message (response : Protocol.Job_dispatch_rpc.Response.t)]
;;

let () =
  let open Command.Let_syntax in
  Command.async' ~summary:"Controller"
    [%map_open
     let config_filename =
       flag "-filename" (required file) ~doc:"PATH to config file"
     in
     fun () ->
       let open Deferred.Let_syntax in
       let%bind config =
         Reader.load_sexp_exn config_filename [%of_sexp: Config.t]
       in
       let worker_host = "0.0.0.0" in
       let worker_port = config.worker_port in
       Tcp.with_connection
         (Tcp.to_host_and_port worker_host worker_port)
         ~timeout:(Time.Span.of_int_sec 1)
         (fun _ r w ->
            match%bind
              Rpc.Connection.create r w ~connection_state:(fun _ -> ())
            with
            | Error exn -> raise exn
            | Ok conn -> do_something conn)]
  |> Command.run
