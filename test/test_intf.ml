open Async

module type S = sig
  val name : string

  val run : unit -> unit Deferred.t
end
