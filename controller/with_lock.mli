(* For restricting access to file system resources. For normal locks, DONT
 * use locks. Async gurantees that code between two bind operators will
 * be reace-free.
 *
 * *)

open Core
open Async

val run
   : ?name: string
  -> Nano_mutex.t
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t
