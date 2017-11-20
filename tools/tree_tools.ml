open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree

let command_pp =
  let open Command.Let_syntax in
  Command.async' ~summary:"pp"
    [%map_open
     let file = anon ("filename" %: string) in
     fun () ->
       let open Deferred.Let_syntax in
       let%bind tree =
         Reader.load_sexp_exn file [%of_sexp: Inlining_tree.Top_level.t]
       in
       List.iter tree ~f:(fun t -> pp t ~indent:0);
       Deferred.unit
    ]

let () =
  Command.group ~summary:"Hello"
    [("pp", command_pp)]
  |> Command.run
