(** Non experiment specific utilities **)

open Core
open Async

include module type of Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree
module Config = Protocol.Config
module Info_rpc = Protocol.Info_rpc
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc
module Relpath = Protocol.Relpath
module Execution_stats = Protocol.Execution_stats
module Results = Protocol.Results
module Work_unit_id = Results.Work_unit_id

val set_shell_defaults : ?echo: bool -> ?verbose: bool -> unit -> unit

val shell 
   : ?env:(string * string) list
  -> ?echo:bool
  -> ?verbose:bool
  -> dir:string
  -> string
  -> string list
  -> unit Core_kernel.Or_error.t Async.Monitor.Deferred.t

(** Remove duplicates from the list of decisions **)
val filter_v0_decisions
   : Data_collector.V0.t list
  -> Data_collector.V0.t list

val lift_deferred : 'a Deferred.t -> 'a Deferred.Or_error.t

val geometric_mean : float list -> float

val arithmetic_mean : float list -> float

val standard_deviation : float list -> float

val gmean_exec_time : Execution_stats.t -> Time.Span.t
