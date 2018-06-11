open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Data_collector = Data_collector.V1
module Inlining_tree = Protocol.Inlining_tree.V1
module Function_metadata = Data_collector.Function_metadata

let command_path_patching =
  let open Command.Let_syntax in
  Command.async ~summary:"path patching"
    [%map_open
      let reference = flag "-reference" (required string) ~doc:"Aa"
      and output_file = flag "-output" (optional_with_default "/dev/stdout" string) ~doc:"Aa"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind reference_tree =
          Reader.load_sexp_exn reference [%of_sexp: Inlining_tree.Top_level.t]
        in
        let replace_closure_origin (function_metadata : Function_metadata.t) =
          let closure_origin = Option.value_exn function_metadata.opt_closure_origin in
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
;;

let command_path_patching_on_decisions =
  let open Command.Let_syntax in
  Command.async ~summary:"path patching"
    [%map_open
      let reference = flag "-reference" (required string) ~doc:"Aa"
      and output_file = flag "-output" (optional_with_default "/dev/stdout" string) ~doc:"Aa"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind reference_decisions =
          Reader.load_sexp_exn reference [%of_sexp: Data_collector.Decision.t list]
        in
        let replace_closure_origin (function_metadata : Function_metadata.t) =
          if Closure_origin.equal Closure_origin.unknown function_metadata.closure_origin
          then function_metadata
          else
            let closure_origin = Option.value_exn function_metadata.opt_closure_origin in
            let opt_closure_origin = None in
            { function_metadata with opt_closure_origin; closure_origin; }
        in
        let patched_decisions =
          List.map reference_decisions ~f:(fun decision ->
              let metadata = replace_closure_origin decision.metadata in
              let trace = List.map decision.trace ~f:(fun trace_item ->
                  match trace_item with
                  | Enter_decl decl -> 
                    let source = Option.map ~f:replace_closure_origin decl.source in
                    let declared = replace_closure_origin decl.declared in
                    Data_collector.Trace_item.Enter_decl { source; declared; }
                  | At_call_site acs ->
                    let source = Option.map ~f:replace_closure_origin acs.source in
                    let applied = replace_closure_origin acs.applied in
                    At_call_site { acs with source; applied; })
              in
              { decision with metadata; trace; })
        in
        let sexp = [%sexp_of: Data_collector.Decision.t list] patched_decisions in 
        let%bind wrt = Writer.open_file output_file in
        Writer.write wrt (Sexp.to_string sexp);
        Deferred.unit]
;;

let loop_lines rdr ~f =
  let idx = ref (-1) in
  let pipe = Async.Reader.lines rdr in
  Async.Pipe.iter pipe ~f:(fun line ->
    idx := !idx + 1;
    f ~idx:(!idx) line)
;;

let command_concat_features =
  let open Command.Let_syntax in
  Command.async ~summary:"Does work"
    [%map_open
      let output = flag "-output" (required string) ~doc:"target file" in
      fun () ->
        let open Deferred.Let_syntax in
        let stdin = Lazy.force Async.Reader.stdin in
        let ref_features =
          ref Protocol.Absolute_path.Map.empty
        in
        let%bind () =
          loop_lines stdin ~f:(fun ~idx:_ filename ->
            let (queries : Inlining_query.query list) =
              let ic = Caml.open_in filename in
              let value = Caml.input_value ic in
              Caml.close_in ic;
              value
            in
            List.iter queries ~f:(fun query -> 
              let key =
                Protocol.Absolute_path.of_trace (Common.query_trace query)
                |> Protocol.Absolute_path.expand
              in
              let data = query in
              ref_features :=
                Protocol.Absolute_path.Map.update !ref_features key ~f:(fun _ -> data));
            return ())
        in
        let features = Protocol.Absolute_path.Map.data !ref_features in
        let oc = Caml.open_out output in
        Caml.output_value oc features;
        Caml.close_out oc;
        Log.Global.info "Extracted %d features" (List.length features);
        Deferred.unit]
;;

let command_concat_queries =
  let open Command.Let_syntax in
  Command.async ~summary:"concatenate inlining queries from files given in stdin"
    [%map_open
      let output = flag "-output" (required string) ~doc:"target file"
      and filelist = flag "-filelist" (required string) ~doc:"File list"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind filelist = Async.Reader.file_lines filelist in
        let%bind queries =
          let rdr = Io_helper.load_queries ~allow_repeat:`No ~filelist in
          Async.Pipe.fold rdr ~init:[] ~f:(fun accum query ->
            return (query :: accum))
        in
        let oc = Caml.open_out output in
        Caml.output_value oc queries;
        Caml.close_out oc;
        Log.Global.info "Extracted %d inlining queries" (List.length queries);
        Deferred.unit]
;;

let stdlib_names =
  [ "arg";
    "arrayLabels";
    "array";
    "buffer";
    "bytesLabels";
    "bytes";
    "callback";
    "camlinternalFormatBasics";
    "camlinternalFormat";
    "camlinternalLazy";
    "camlinternalMod";
    "camlinternalOO";
    "char";
    "complex";
    "digest";
    "ephemeron";
    "filename";
    "format";
    "gc";
    "genlex";
    "hashtbl";
    "int32";
    "int64";
    "lazy";
    "lexing";
    "listLabels";
    "list";
    "map";
    "marshal";
    "moreLabels";
    "nativeint";
    "obj";
    "oo";
    "parsing";
    "pervasives";
    "printexc";
    "printf";
    "queue";
    "random";
    "scanf";
    "set";
    "sort";
    "spacetime";
    "stack";
    "stdLabels";
    "stream";
    "stringLabels";
    "string";
    "sys";
    "uchar";
    "weak";
  ]
  |> List.map ~f:(fun s -> "caml" ^ String.capitalize s)
  |> String.Set.of_list

(* unstable-closure-origin incorrectly have some standard libraries incremented by 1 *)

let already_fixed =
  [("camlCamlinternalFormat", "make_ignored_param", 5285);
   ("camlList", "iter", 504);
   ("camlList", "fold_left", 551);
   ("camlArray", "fill", 580);
   ("camlList", "length", 175);
   ("camlPrintf", "printf", 159);
   ("camlPervasives", "open_out", 628);
   ("camlGc", "print_stat", 25);
   ("camlPervasives", "@", 588);
   ("camlArray", "init", 98);
  ]
;;

let command_clean_decisions =
  let open Command.Let_syntax in
  Command.async ~summary:"concatenate inlining queries from files given in stdin"
    [%map_open
      let filename =
        flag "-decision" (required string) ~doc:"decision file that needs patching"
      in
      let looks_fixed co =
        List.exists already_fixed (fun (linkage_name, name, stamp) ->
          let (my_stamp : int) = Obj.obj (Obj.field (Obj.repr co) 2) in
          let my_linkage_name =
            Closure_origin.get_compilation_unit co
            |> Compilation_unit.get_linkage_name
            |> Fyp_compiler_lib.Linkage_name.to_string
          in
          let my_name = Closure_origin.get_name co in
          my_stamp = stamp &&
          my_name = name &&
          my_linkage_name = linkage_name)
      in
      let fix_closure_origin co =
        if looks_fixed co then begin
          failwithf "%s looks fixed. terminating." (Format.asprintf "%a" Closure_origin.print co) ();
        end;
        let (stamp : int) = Obj.obj (Obj.field (Obj.repr co) 2) in
        let ret = Obj.dup (Obj.repr co) in
        Obj.set_field ret 2 (Obj.repr (stamp - 1));
        Obj.obj ret
      in
      let fix_closure_origin co =
        let cu = Closure_origin.get_compilation_unit co in
        let linkage_name = Compilation_unit.get_linkage_name cu in
        let linkage_name = (Fyp_compiler_lib.Linkage_name.to_string linkage_name) in
        if String.Set.mem stdlib_names linkage_name then begin
          let new_co = fix_closure_origin co in
          printf "MOD %s -> %s\n" 
            (Format.asprintf "%a" Fyp_compiler_lib.Closure_origin.print co)
            (Format.asprintf "%a" Fyp_compiler_lib.Closure_origin.print new_co);
          new_co
        end else begin
          co
        end
      in
      let fix_function_metadata (fm : Data_collector.Function_metadata.t) =
        let open Data_collector.Function_metadata in
        { fm with
          closure_origin = fix_closure_origin fm.closure_origin;
          opt_closure_origin = Option.map ~f:fix_closure_origin fm.opt_closure_origin;
        }
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind decisions =
          Reader.load_sexp_exn filename [%of_sexp: Data_collector.Decision.t list]
        in
        let new_decisions =
          List.map decisions ~f:(fun decision ->
            let open Data_collector.Trace_item in
            let trace =
              List.map decision.trace ~f:(fun trace_item ->
                match trace_item with
                | Enter_decl { source; declared } ->
                  Enter_decl {
                    source  = Option.map source ~f:fix_function_metadata;
                    declared = fix_function_metadata declared;
                  }
                | At_call_site { source; applied; apply_id; } ->
                  At_call_site {
                    source  = Option.map source ~f:fix_function_metadata;
                    applied = fix_function_metadata applied;
                    apply_id;
                  })
            in
            { decision with trace })
        in
        Writer.save_sexp filename
          ([%sexp_of: Data_collector.Decision.t list] new_decisions)
    ]
;;


let () =
  Command.group ~summary:"Data cleaner" [
    ("clean-decisions", command_clean_decisions);
    ("path-patching", command_path_patching);
    ("path-patching-on-decisions", command_path_patching_on_decisions);
    ("concat-features", command_concat_features);
    ("concat-queries", command_concat_queries);
  ]
  |> Command.run
;;
