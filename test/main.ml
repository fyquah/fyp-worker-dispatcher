open Core
open Async

open Regex_test
open Inlining_tree_test

let () =
  Command.async ~summary:"Execute tests"
    (Command.Param.return (fun () ->
      Deferred.unit >>= fun () ->
      Testlib.run ()))
  |> Command.run
;;
