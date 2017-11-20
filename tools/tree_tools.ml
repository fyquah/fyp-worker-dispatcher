open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree

let command_pp =
  let mk_indent indent = String.make indent ' ' in
  let rec pp t ~indent =
    match (t : Inlining_tree.t) with
    | Declaration decl ->
      printf "%s| Declaration { %s }\n"
        (mk_indent indent)
        (Caml.Format.asprintf "%a" Closure_id.print decl.closure);
      List.iter decl.children ~f:(fun a -> pp a ~indent:(indent + 1))
    | Apply_inlined_function inlined ->
      printf "%s| [%d] Inlined { %s }\n"
        (mk_indent indent)
        (Call_site_offset.to_int inlined.offset)
        (Caml.Format.asprintf "%a" Closure_id.print inlined.applied);
      List.iter inlined.children ~f:(fun a -> pp a ~indent:(indent + 1))
    | Apply_non_inlined_function non_inlined ->
      printf "%s| [%d] Not inlined { %s }\n"
        (mk_indent indent)
        (Call_site_offset.to_int non_inlined.offset)
        (Caml.Format.asprintf "%a" Closure_id.print non_inlined.applied)
  in
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
