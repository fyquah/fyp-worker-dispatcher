open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree

module V1 = struct

  open Data_collector.V1

  module Inlining_tree = Inlining_tree.V1

  let command_pp_decisions =
    let open Command.Let_syntax in
    Command.async' ~summary:"pprint decisions"
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

  let command =
    Command.group ~summary:"Tree tools (for v1)"
      [("print-tree", command_pp_tree);
       ("print-decisions", command_pp_decisions);
      ]
end

let () =
  Command.group ~summary:"Tree tools" [("v1", V1.command)]
  |> Command.run
