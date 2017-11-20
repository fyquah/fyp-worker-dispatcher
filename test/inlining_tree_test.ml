open Core
open Async

module Inlining_tree = Protocol.Inlining_tree

module T1 = struct
  let name = "compiler-call-site-offset"

  let run () =
    Deferred.unit
end

let () =
  Testlib.register (module T1);
