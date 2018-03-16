open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree.V1
module Function_metadata = Data_collector.Function_metadata

let command_path_patching =
  let open Command.Let_syntax in
  Command.async' ~summary:"path patching"
    [%map_open
      let dirty = anon ("filename" %: string)
      and reference = flag "-reference" (required string) ~doc:"Aa"
      and output_file = flag "-output" (optional_with_default "/dev/stdout" string) ~doc:"Aa"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind reference_tree =
          Reader.load_sexp_exn reference [%of_sexp: Inlining_tree.Top_level.t]
        in
        let%bind dirty_tree =
          Reader.load_sexp_exn dirty [%of_sexp: Inlining_tree.Top_level.t]
        in
        let replace_closure_origin (function_metadata : Function_metadata.t) =
          let closure_origin = function_metadata.closure_origin in
          let opt_closure_origin = None in
          { function_metadata with opt_closure_origin; closure_origin; }
        in
        let patched_tree =
          Inlining_tree.Top_level.map reference_tree ~f:(function
            | Inlining_tree.Declaration decl ->
              let declared = replace_closure_origin (decl.declared) in
              Inlining_tree.Declaration { decl with declared }
            | Apply_inlined_function inlined ->
              let applied = replace_closure_origin (inlined.applied) in
              Inlining_tree.Apply_inlined_function { inlined with applied }
            | Apply_non_inlined_function not_inlined ->
              let applied = replace_closure_origin (not_inlined.applied) in
              Inlining_tree.Apply_non_inlined_function { not_inlined with applied })
        in
        let sexp = Inlining_tree.Top_level.sexp_of_t patched_tree in 
        let%bind wrt = Writer.open_file output_file in
        Writer.write wrt (Sexp.to_string sexp);
        Deferred.unit]


let () =
  Command.group ~summary:"Data cleaner" [
    ("path-patching", command_path_patching)]
  |> Command.run
;;

