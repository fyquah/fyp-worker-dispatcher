open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree

module V1 = struct

  module Inlining_tree = Inlining_tree.V1
  module Function_metadata = Data_collector.V1

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
         let rec loop ~indent node =
           let space = 
            List.init indent ~f:(fun _ -> " ")  |> String.concat ~sep:""
           in
           match node with
           | Inlining_tree.Declaration decl ->
             Format.printf "%sDECL %a\n" space Closure_origin.print
               decl.declared.closure_origin;
             iterate_children ~indent decl.children
           | Apply_inlined_function inlined ->
             Format.printf "%sINLINE %a\n" space Apply_id.print inlined.apply_id;
             iterate_children ~indent inlined.children
           | Apply_non_inlined_function non_inlined -> 
             Format.printf "%sDONT_INLINE %a\n" space Apply_id.print non_inlined.apply_id

         and iterate_children ~indent children =
           List.iter children ~f:(fun child ->
               loop ~indent:(indent + 1) child)
         in
         iterate_children ~indent:(-1) tree;
         Deferred.unit
      ]

  let command =
    Command.group ~summary:"Tree tools (for v1)"
      [("pp", command_pp)]
end

let () =
  Command.group ~summary:"Tree tools" [("v1", V1.command)]
  |> Command.run
