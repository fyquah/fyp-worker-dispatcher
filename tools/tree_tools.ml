open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree

module V0 = struct
  module Inlining_tree = Inlining_tree.V0

  module Pretty_print = struct
    let rec loop ~buffer ~indent node =
      let space =
       List.init indent ~f:(fun _ -> " ")  |> String.concat ~sep:""
      in
      match node with
      | Inlining_tree.Declaration decl ->
        bprintf buffer "%sDECL(%s)\n" space
          (Format.asprintf "%a" Closure_id.print decl.closure);
        iterate_children ~buffer ~indent decl.children
      | Apply_inlined_function inlined ->
        bprintf buffer "%sINLINE[%d](%s)\n" space
          (Call_site_offset.to_int inlined.offset)
          (Format.asprintf "%a" Closure_id.print inlined.applied);
        iterate_children ~buffer ~indent inlined.children
      | Apply_non_inlined_function non_inlined ->
        bprintf buffer "%sDONT_INLINE[%d](%s)\n" space
          (Call_site_offset.to_int non_inlined.offset)
          (Format.asprintf "%a" Closure_id.print non_inlined.applied);

    and iterate_children ~buffer ~indent children =
      List.iter children ~f:(fun child ->
          loop ~buffer ~indent:(indent + 1) child)
  end

  let command_diff_tree =
    let node_to_string = function
      | Inlining_tree.Declaration declaration ->
        Format.asprintf "DECL(%a)"
          Closure_id.print declaration.closure
      | Apply_inlined_function inlined_function ->
        Format.asprintf "INLINED[%d](%a)"
          (Call_site_offset.to_int inlined_function.offset)
          Closure_id.print inlined_function.applied
      | Apply_non_inlined_function non_inlined_function ->
        Format.asprintf "CALL[%d](%a)"
          (Call_site_offset.to_int non_inlined_function.offset)
          Closure_id.print non_inlined_function.applied
    in
    let open Command.Let_syntax in
    Command.async ~summary:"diff_tree"
      [%map_open
       let file_a = anon ("filename" %: string)
       and file_b = anon ("filename" %: string) in
       fun () ->
         let open Deferred.Let_syntax in
         let%bind tree_a =
           Reader.load_sexp_exn file_a [%of_sexp: Inlining_tree.Top_level.t]
         in
         let%bind tree_b =
           Reader.load_sexp_exn file_b [%of_sexp: Inlining_tree.Top_level.t]
         in
         let diffs = Inlining_tree.diff ~left:tree_a ~right:tree_b in
         List.iteri diffs ~f:(fun i diff ->
             printf "Diff %d\n" i;
             let common_ancestor = diff.common_ancestor in
             printf "| common ancestor: \n";
             List.iter common_ancestor ~f:(fun node ->
                 printf "|  %s\n" (node_to_string node));
             let buffer = Buffer.create 10 in
             bprintf buffer "| left:\n";
             Pretty_print.iterate_children ~buffer ~indent:3
               (List.map diff.left ~f:(fun (`Left node) -> node));
             bprintf buffer "| right:\n";
             Pretty_print.iterate_children ~buffer ~indent:3
               (List.map diff.right ~f:(fun (`Right node) -> node));
             printf "%s" (Buffer.contents buffer)
           );
         Deferred.unit
      ]
  ;;


  let command_pp_tree =
    let open Command.Let_syntax in
    Command.async ~summary:"pp"
      [%map_open
       let file = anon ("filename" %: string) in
       fun () ->
         let open Deferred.Let_syntax in
         let%bind tree =
           Reader.load_sexp_exn file [%of_sexp: Inlining_tree.Top_level.t]
         in
         let buffer = Buffer.create 10 in
         Pretty_print.iterate_children ~buffer ~indent:(-1) tree;
         printf "%s" (Buffer.contents buffer);
         Deferred.unit
      ]
  ;;

  let command =
    Command.group ~summary:"Tree tools (for v0 - DEPRECATED)"
      [("print-tree", command_pp_tree);
       ("diff-tree", command_diff_tree);
      ]
  ;;
end

module V1 = struct

  open Data_collector.V1

  module Inlining_tree = Inlining_tree.V1

  module Pretty_print = struct
    let rec loop ~buffer ~indent node =
      let space =
       List.init indent ~f:(fun _ -> " ")  |> String.concat ~sep:""
      in
      match node with
      | Inlining_tree.Declaration decl ->
        bprintf buffer "%sDECL(%s)\n" space
          (Format.asprintf "%a" Function_metadata.print decl.declared);
        iterate_children ~buffer ~indent decl.children
      | Apply_inlined_function inlined ->
        bprintf buffer "%sINLINE[%s](%s)\n" space
          (Format.asprintf "%a" Apply_id.print inlined.apply_id)
          (Format.asprintf "%a" Function_metadata.print inlined.applied);
        iterate_children ~buffer ~indent inlined.children
      | Apply_non_inlined_function non_inlined ->
        bprintf buffer "%sDONT_INLINE[%s](%s)\n" space
          (Format.asprintf "%a" Apply_id.print non_inlined.apply_id)
          (Format.asprintf "%a" Function_metadata.print non_inlined.applied);

    and iterate_children ~buffer ~indent children =
      List.iter children ~f:(fun child ->
          loop ~buffer ~indent:(indent + 1) child)
  end

  let command_pp_decisions =
    let open Command.Let_syntax in
    Command.async ~summary:"pprint decisions"
      [%map_open
        let file = anon("filename" %: string) in
        fun () ->
          let open Deferred.Let_syntax in
          let%map decisions =
            Reader.load_sexp_exn file [%of_sexp: Decision.t list]
          in
          List.iter decisions ~f:(fun decision ->
              printf "[round %d]\n" decision.round;
              printf "TOP_LEVEL\n";
              begin match List.hd_exn decision.trace with
              | Trace_item.Enter_decl _ -> assert false
              | At_call_site acs ->
                assert (
                  Apply_id.equal acs.apply_id decision.apply_id
                  && Function_metadata.compare acs.applied decision.metadata = 0
                )
              end;
              let trace = List.tl_exn decision.trace in
              List.iter (List.rev trace) ~f:(fun trace_item ->
                  match trace_item with
                  | Trace_item.Enter_decl { source = _; declared } ->
                    printf "--declares--> %s\n" (
                      Format.asprintf "Decl{%a}" Closure_origin.print declared.closure_origin)
                  | Trace_item.At_call_site { apply_id; applied; source = _; } ->
                    printf "--inlines--> %s\n" (
                      Format.asprintf "[%a](%a)"
                        Apply_id.print apply_id
                        Closure_origin.print applied.closure_origin));
              begin match decision.action with
              | Action.Inline -> printf "--inlines--> "
              | Action.Apply -> printf "--calls--> "
              end;
              printf "%s\n" (
                Format.asprintf "[%a](%a)"
                  Apply_id.print decision.apply_id
                  Closure_origin.print decision.metadata.closure_origin))]
  ;;

  let command_pp_tree =
    let open Command.Let_syntax in
    Command.async ~summary:"pp"
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
             Format.printf "%sDECL(%a)\n" space Closure_origin.print
               decl.declared.closure_origin;
             iterate_children ~indent decl.children
           | Apply_inlined_function inlined ->
             Format.printf "%sINLINE[%a](%a)\n" space
               Apply_id.print inlined.apply_id
               Closure_origin.print inlined.applied.closure_origin;
             iterate_children ~indent inlined.children
           | Apply_non_inlined_function non_inlined ->
             Format.printf "%sDONT_INLINE(%a) %a\n" space
               Apply_id.print non_inlined.apply_id
               Closure_origin.print non_inlined.applied.closure_origin;

         and iterate_children ~indent children =
           List.iter children ~f:(fun child ->
               loop ~indent:(indent + 1) child)
         in
         iterate_children ~indent:(-1) tree;
         Deferred.unit
      ]
  ;;

  let command_diff_tree =
    let node_to_string = function
      | Inlining_tree.Declaration declaration ->
        Format.asprintf "DECL(%a)"
          Function_metadata.print declaration.declared
      | Apply_inlined_function inlined_function ->
        Format.asprintf "INLINED[%a](%a)"
          Apply_id.print inlined_function.apply_id
          Function_metadata.print inlined_function.applied
      | Apply_non_inlined_function non_inlined_function ->
        Format.asprintf "CALL[%a](%a)"
          Apply_id.print non_inlined_function.apply_id
          Function_metadata.print non_inlined_function.applied
    in
    let open Command.Let_syntax in
    Command.async' ~summary:"diff_tree"
      [%map_open
       let file_a = anon ("filename" %: string)
       and file_b = anon ("filename" %: string) in
       fun () ->
         let open Deferred.Let_syntax in
         let%bind tree_a =
           Reader.load_sexp_exn file_a [%of_sexp: Inlining_tree.Top_level.t]
         in
         let%bind tree_b =
           Reader.load_sexp_exn file_b [%of_sexp: Inlining_tree.Top_level.t]
         in
         let diffs = Inlining_tree.diff ~left:tree_a ~right:tree_b in
         List.iteri diffs ~f:(fun i diff ->
             printf "Diff %d\n" i;
             let common_ancestor = diff.common_ancestor in
             printf "| common ancestor: \n";
             List.iter common_ancestor ~f:(fun node ->
                 printf "|  %s\n" (node_to_string node));
             let buffer = Buffer.create 10 in
             bprintf buffer "| left:\n";
             Pretty_print.iterate_children ~buffer ~indent:3
               (List.map diff.left ~f:(fun (`Left node) -> node));
             bprintf buffer "| right:\n";
             Pretty_print.iterate_children ~buffer ~indent:3
               (List.map diff.right ~f:(fun (`Right node) -> node));
             printf "%s" (Buffer.contents buffer)
           );
         Deferred.unit
      ]
  ;;

  let command =
    Command.group ~summary:"Tree tools (for v1)"
      [("print-tree", command_pp_tree);
       ("print-decisions", command_pp_decisions);
       ("diff-tree", command_diff_tree);
      ]
end

let () =
  Command.group ~summary:"Tree tools" [
    ("v0", V0.command);
    ("v1", V1.command);
  ]
  |> Command.run
