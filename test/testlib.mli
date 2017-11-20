open Async

val register : (module Test_intf.S) -> unit

val run : unit -> unit Deferred.t
