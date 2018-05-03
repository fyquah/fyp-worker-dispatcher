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
          (Function_metadata.pprint decl.declared);
        iterate_children ~buffer ~indent decl.children
      | Apply_inlined_function inlined ->
        bprintf buffer "%sINLINE[%s](%s)\n" space
          (Format.asprintf "%a" Apply_id.print inlined.apply_id)
          (Function_metadata.pprint inlined.applied);
        iterate_children ~buffer ~indent inlined.children
      | Apply_non_inlined_function non_inlined ->
        bprintf buffer "%sDONT_INLINE[%s](%s)\n" space
          (Format.asprintf "%a" Apply_id.print non_inlined.apply_id)
          (Function_metadata.pprint non_inlined.applied);

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
              | Action.Specialise -> printf "--specialises--> "
              end;
              printf "%s\n" (
                Format.asprintf "[%a](%a)"
                  Apply_id.print decision.apply_id
                  Closure_origin.print decision.metadata.closure_origin))]
  ;;

  let node_to_string = function
    | Inlining_tree.Declaration declaration ->
      Format.asprintf "DECL(%a)"
        Closure_origin.print declaration.declared.closure_origin
    | Apply_inlined_function inlined_function ->
      Format.asprintf "INLINE<%s>(%a)"
        (Apply_id.Path.to_string (Apply_id.to_path inlined_function.apply_id))
        Closure_origin.print inlined_function.applied.closure_origin
    | Apply_non_inlined_function non_inlined_function ->
      Format.asprintf "DONT_INLINE<%s>(%a)"
        (Apply_id.Path.to_string (Apply_id.to_path non_inlined_function.apply_id))
        Closure_origin.print non_inlined_function.applied.closure_origin
  ;;

  let command_pp_tree_from_decisions =
    let open Command.Let_syntax in
    Command.async ~summary:"pp"
      [%map_open
       let file = anon ("filename" %: string) in
       fun () ->
         let open Deferred.Let_syntax in
         let%bind decisions =
           Reader.load_sexp_exn file [%of_sexp: Decision.t list]
         in
         let tree = Inlining_tree.build decisions in
         let buffer = Buffer.create 10 in
         Pretty_print.iterate_children ~buffer ~indent:(-1) tree;
         printf "%s" (Buffer.contents buffer);
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
         let rec loop ~indent node =
           let space =
            List.init indent ~f:(fun _ -> " ")  |> String.concat ~sep:""
           in
           printf "%s%s\n" space (node_to_string node);
           match node with
           | Inlining_tree.Declaration decl ->
             iterate_children ~indent decl.children
           | Apply_inlined_function inlined ->
             iterate_children ~indent inlined.children
           | Apply_non_inlined_function _ -> ()

         and iterate_children ~indent children =
           List.iter children ~f:(fun child ->
               loop ~indent:(indent + 1) child)
         in
         iterate_children ~indent:(-1) tree;
         Deferred.unit
      ]
  ;;

  let command_pp_expanded =
    let open Command.Let_syntax in
    Command.async ~summary:"pp"
      [%map_open
       let file = anon ("filename" %: string) in
       fun () ->
         let open Deferred.Let_syntax in
         let%bind tree =
           Reader.load_sexp_exn file [%of_sexp: Inlining_tree.Top_level.Expanded.t]
         in
         let buffer = Buffer.create 1000 in
         Inlining_tree.Top_level.Expanded.pprint buffer tree;
         printf "%s" (Buffer.contents buffer);
         Deferred.unit
      ]
  ;;

  let command_diff_tree =
    let open Command.Let_syntax in
    Command.async ~summary:"diff_tree"
      [%map_open
       let file_a = anon ("filename" %: string)
       and file_b = anon ("filename" %: string)
       and loose = flag "-loose" no_arg ~doc:"loose"
       and remove_unknowns = flag "-remove-unknowns" no_arg ~doc:"hello"
       and remove_empty_decl = flag "-remove-empty-decl" no_arg ~doc:"hello"
       in
       fun () ->
         let open Deferred.Let_syntax in
         let%bind tree_a =
           Reader.load_sexp_exn file_a [%of_sexp: Inlining_tree.Top_level.t]
         in
         let%bind tree_b =
           Reader.load_sexp_exn file_b [%of_sexp: Inlining_tree.Top_level.t]
         in
         let dynamic_dispatch a b =
           if a then b else Fn.id
         in
         let preprocess tree =
           tree
           |> dynamic_dispatch remove_unknowns
              Inlining_tree.Top_level.remove_unknowns
           |> dynamic_dispatch remove_empty_decl
              Inlining_tree.Top_level.remove_empty_declarations
         in
         let loose =
           if loose then Some ()
           else None
         in
         let tree_a = preprocess tree_a in
         let tree_b = preprocess tree_b in
         let diffs = Inlining_tree.diff ?loose ~left:tree_a ~right:tree_b in
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

  let command_decisions_to_tree =
    let open Command.Let_syntax in
    Command.async ~summary:"convert compiler decisions to tree"
      [%map_open
        let input_file = anon ("filename" %: string)
        and output_file =  flag "-output" (required string) ~doc:"STRING"
        and expand = flag "-expand" no_arg ~doc:"FLAG" in
        fun () ->
          let open Deferred.Let_syntax in
          let%bind decisions =
            Reader.load_sexp_exn input_file [%of_sexp: Decision.t list]
          in
          let tree = Inlining_tree.build decisions in
          let sexp =
            if expand then
              Inlining_tree.Top_level.expand tree
              |> Inlining_tree.Top_level.Expanded.sexp_of_t
            else
              Inlining_tree.Top_level.sexp_of_t tree
          in
          let%map wrt = Writer.open_file output_file in
          Writer.write wrt (Sexp.to_string sexp)
      ]
  ;;

  let command_check_soundness =
    let open Command.Let_syntax in
    Command.async_or_error ~summary:"convert compiler decisions to tree"
      [%map_open
        let compiled = anon ("candidate" %: string)
        and reference =  flag "-reference" (required string) ~doc:"STRING"
        and loose = flag "-loose" no_arg ~doc:"loose"
        and remove_unknowns = flag "-remove-unknowns" no_arg ~doc:"hello"
        and remove_empty_decl = flag "-remove-empty-decl" no_arg ~doc:"hello"
        in
        fun () ->
          let open Deferred.Or_error.Let_syntax in
          let%bind compiled =
            Reader.load_sexp compiled [%of_sexp: Inlining_tree.Top_level.t]
          in
          let%bind reference =
            Reader.load_sexp reference [%of_sexp: Inlining_tree.Top_level.t]
          in
          let dynamic_dispatch a b =
            if a then b else Fn.id
          in
          let preprocess tree =
            tree
            |> dynamic_dispatch remove_unknowns
               Inlining_tree.Top_level.remove_unknowns
            |> dynamic_dispatch remove_empty_decl
               Inlining_tree.Top_level.remove_empty_declarations
          in
          let compiled = preprocess compiled in
          let reference = preprocess reference in
          let loose = if loose then Some () else None in
          let soundness =
            Inlining_tree.Top_level.check_soundness ?loose ~reference ~compiled ()
          in
          if soundness.is_sound then begin
            printf "Num nodes in reference = %d\n"
              soundness.total_nodes_in_reference;
            printf "Num matched nodes in reference = %d\n"
            soundness.matched_reference_nodes;
            Deferred.Or_error.return ()
          end else begin
            Deferred.Or_error.error_string "Not sound"
          end]
  ;;
  
  let command_expanded_to_decisions =
    let open Command.Let_syntax in
    Command.async ~summary:"bla" [%map_open
      let input = anon ("filename" %: string)
      and output  = flag "-output"  (required string) ~doc:"output" in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind tree = 
          Reader.load_sexp_exn input
            [%of_sexp: Inlining_tree.Top_level.Expanded.t]
        in
        let overrides =
          Inlining_tree.Top_level.Expanded.to_override_rules tree
        in
        let sexp = [%sexp_of: Data_collector.Overrides.t] overrides in
        let%map wrt = Writer.open_file output in
        Writer.write wrt (Sexp.to_string sexp)]
  ;;

  let command =
    Command.group ~summary:"Tree tools (for v1)"
      [("print-tree", command_pp_tree);
       ("print-expanded", command_pp_expanded);
       ("print-tree-from-decisions", command_pp_tree_from_decisions);
       ("print-decisions", command_pp_decisions);
       ("diff-tree", command_diff_tree);
       ("decisions-to-tree", command_decisions_to_tree);
       ("check-soundness", command_check_soundness);
       ("expanded-to-decisions", command_expanded_to_decisions);
      ]
end

let () =
  Command.group ~summary:"Tree tools" [
    ("v0", V0.command);
    ("v1", V1.command);
  ]
  |> Command.run
