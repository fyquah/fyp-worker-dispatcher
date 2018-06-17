open Core
open Async
open Common


(* All relevant data is saved into [data_directory]/[step]/[sub_id]/ *)
module Work_unit : sig
  type t =
    { path_to_bin : string;
      step        : [ `Step of int | `Initial ];
      sub_id      : [ `Sub_id of int | `Current ];
    }
  [@@deriving sexp]
end

module Dump_utils : sig

  val set_controller_rundir : string -> unit

  val execution_dump_directory
     : step: [ `Step of int | `Initial ]
    -> sub_id: [ `Sub_id of int | `Current ]
    -> string
end



module Initial_state : sig
  type t =
    { v0_decisions : Data_collector.V0.t list;
      v1_decisions : Data_collector.V1.Decision.t list;
      path_to_bin  : string;
    }
end

module Worker_connection : sig
  type 'a rpc_conn =
    { hostname : string;
      socket : ([`Active], 'a) Socket.t;
      reader : Reader.t;
      writer : Writer.t;
      rpc_connection : Rpc.Connection.t
    }

  type ssh_conn =
    { user: string;
      hostname: string;
      rundir: string;
      processor: int option;
    }

  type 'a t =
    | Rpc : 'a rpc_conn -> 'a t
    | Ssh : ssh_conn -> 'a t

  val hostname : 'a t -> string

  val lockname : 'a t -> string

  val processor : 'a t -> int option
end

module Scheduler : sig
  type ('a, 'b, 'c) t

  val num_workers : ('a, 'b, 'c) t -> int

  val create
    : 'a Worker_connection.t list
    -> process:('a Worker_connection.t -> 'b -> 'c Deferred.t)
    -> ('b, 'c, 'a) t Deferred.t

  val dispatch : ('a, 'b, 'c) t -> 'a -> 'b Deferred.t

  val terminate : ('a, 'b, 'c) t -> unit
end

val read_decisions
   : round: int
  -> exp_dir: string
  -> module_paths: string list
  -> (Data_collector.V0.t list * Data_collector.V1.Decision.t list) Deferred.Or_error.t

val get_initial_state
   : ?env:(string * string) list
  -> round: int
  -> module_paths: string list
  -> bin_name:string
  -> exp_dir:string
  -> base_overrides: Data_collector.V0.t list
  -> unit
  -> Initial_state.t option Deferred.Or_error.t


val run_binary_on_worker
  : num_runs:Core.Int.t ->
  processor:int Core.Option.t ->
  hostname:string ->
  conn:'a Worker_connection.t ->
  path_to_bin:string ->
  bin_args:Core.String.t
  -> dump_dir : string
  -> bin_files: Core.String.t list (* Absolute path!!!! *)
  -> (Protocol.Execution_stats.t, Core_kernel__.Error.t) Async.Deferred.Result.t

val init_connection
   : hostname: string
  -> worker_config: Protocol.Config.worker_config
  -> Socket.Address.Inet.t Worker_connection.t Deferred.Or_error.t

val process_work_unit
   : num_runs:int
  -> bin_args:string
  -> bin_files:string list
  -> [< Async.Socket.Address.t ] Worker_connection.t
  -> Work_unit.t
  -> Protocol.Execution_stats.t Deferred.Or_error.t

(** Copies compilation artifacts into [dump_dir] and store them as a .tar **)
val copy_compilation_artifacts
   : exp_dir: string
  -> dump_dir: string
  -> abs_path_to_binary: string
  -> unit Deferred.Or_error.t

(** Calls make clean and make all, then calls [copy_compilation_artifacts] **)
val compile_binary
   : dir:string
  -> bin_name:string
  -> write_overrides:(string -> unit Deferred.Or_error.t)
  -> dump_directory: string
  -> string Deferred.Or_error.t

val run_in_all_workers
   : scheduler:(Work_unit.t, Common.Execution_stats.t Core_kernel.Or_error.t, 'a) Scheduler.t
  -> times:int
  -> config:Common.Config.t
  -> path_to_bin:string
  -> Common.Execution_stats.t Deferred.Or_error.t

val with_file_lock : string -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t
