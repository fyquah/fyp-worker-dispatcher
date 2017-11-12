open Core
open Async

module Feature_extractor = struct
  include Fyp_compiler_lib.Feature_extractor

  let sexp_of_call_context x =
    let s =
      match x with
      | Conditional_branch -> "Conditional_branch"
      | String_switch_branch -> "String_switch_branch"
      | Switch_int_branch -> "Switch_int_branch"
      | Switch_block_branch -> "Switch_block_branch"
      | Switch_failaction_branch -> "Switch_failaction_branch"
      | Imperative_loop -> "Imperative_loop"
      | In_try_block -> "In_try_block"
      | In_catch_block -> "In_catch_block"
      | Inlined_function -> "Inlined_function"
      | In_function_declaration -> "In_function_declaration"
    in
    Sexp.Atom s
  ;;

  let sexp_of_t a =
    let f x y ~f = Sexp.List [ Sexp.Atom x; f y ] in
    let fi x y = Sexp.List [ Sexp.Atom x; Int.sexp_of_t y ] in
    let fb x y = Sexp.List [ Sexp.Atom x; Bool.sexp_of_t y ] in
    let call_context_stack_sexp =
      Sexp.List [
        Sexp.Atom "call_context_stack";
        [%sexp_of: call_context list ] a.call_context_stack;
      ]
    in
    Sexp.List [
      fi "params" a.params;
      fi "bound_vars_to_symbol" a.bound_vars_to_symbol;
      fi "assign" a.assign;
      fi "bound_vars_to_mutable" a.bound_vars_to_mutable;
      fi "bound_vars" a.bound_vars;
      fi "free_vars" a.free_vars;
      fi "free_symbols" a.free_symbols;
      fi "set_of_closures" a.set_of_closures;
      fb "is_a_functor" a.is_a_functor;
      fi "non_specialized_args" a.non_specialized_args;
      fi "specialized_args" a.specialized_args;
      fi "size_before_simplify" a.size_before_simplify;
      fi "size_after_simplify" a.size_after_simplify;
      fi "underlying_direct_applications" a.underlying_direct_applications;
      fi "underlying_indirect_applications" a.underlying_indirect_applications;
      fb "is_recursive" a.is_recursive;
      f ~f:[%sexp_of: float option]
        "expected_allocations" a.expected_allocations;
      fb "is_annonymous" a.is_annonymous;
      fi "if_then_else" a.if_then_else;
      fi "switch" a.switch;
      fi "string_switch" a.string_switch;
      call_context_stack_sexp;
      fb "direct_call" a.direct_call;
      fb "recursive_call" a.recursive_call;
      fi "inlining_depth" a.inlining_depth;
      fi "closure_depth" a.closure_depth;
      f ~f:[%sexp_of: int option]
        "original_function_size" a.original_function_size;
      f ~f:[%sexp_of: int option]
        "original_bound_vars" a.original_bound_vars;
      fi "flambda_round" a.flambda_round;
    ]
  ;;

  let to_csv_row t =
    let fi = Int.to_string in
    let ff = Float.to_string in
    let fb x = if x then fi 1 else fi 0 in
    let fc i =
      let c = List.nth t.call_context_stack i in
      let value =
        match c with
        | None -> 0
        | Some Conditional_branch -> 1
        | Some String_switch_branch -> 2
        | Some Switch_int_branch -> 3
        | Some Switch_block_branch -> 4
        | Some Switch_failaction_branch -> 5
        | Some Imperative_loop -> 6
        | Some In_try_block -> 7
        | Some In_catch_block -> 8
        | Some Inlined_function -> 9
        | Some In_function_declaration -> 10
      in
      fi value
    in
    [ ("params", fi t.params);
      ("bound_vars_to_symbol", fi t.bound_vars_to_symbol);
      ("assign", fi t.assign);
      ("bound_vars_to_mutable", fi t.bound_vars_to_mutable);
      ("free_vars", fi t.free_vars);
      ("free_symbols", fi t.free_symbols);
      ("set_of_closures", fi t.set_of_closures);
      ("is_a_functor", fb t.is_a_functor);
      ("non_specialized_args", fi t.non_specialized_args);
      ("specialized_args", fi t.specialized_args);
      ("size_before_simplify", fi t.size_before_simplify);
      ("size_after_simplify", fi t.size_after_simplify);
      ("underlying_direct_applications", fi t.underlying_direct_applications);
      ("underlying_indirect_applications", fi t.underlying_indirect_applications);
      ("is_recursive", fb t.is_recursive);
      ("expected_allocations", ff (Option.value ~default:0.0 t.expected_allocations));
      ("is_annonymous", fb t.is_annonymous);
      ("if_then_else", fi t.if_then_else);
      ("switch", fi t.switch);
      ("string_switch", fi t.string_switch);

      ("call_context_0", fc 0);
      ("call_context_1", fc 1);
      ("call_context_2", fc 2);
      ("call_context_3", fc 3);
      ("call_context_4", fc 4);

      ("direct_call", fb t.direct_call);
      ("recursive_call", fb t.recursive_call);
      ("inlining_depth", fi t.inlining_depth);
      ("closure_depth", fi t.closure_depth);
      ("in_recursive_function", fb t.in_recursive_function);
      ("original_function_size_exists",
       fb (Option.is_some t.original_function_size));
      ("original_function_size",
       fi (Option.value ~default:0 t.original_function_size));
      ("original_bound_vars_exists",
       fb (Option.is_some t.original_bound_vars));
      ("original_bound_vars",
       fi (Option.value ~default:0 t.original_bound_vars));
      ("flambda_round", fi t.flambda_round);
      ("flambda_tries", fb t.flambda_tries);
    ]
    |> String.Map.of_alist_exn
end

let () =
  let open Command.Let_syntax in
  Command.async' ~summary:"hello"
    [%map_open
      let filelist = flag "-filelist" (required string) ~doc:"FILE"
      and filelist_root = flag "-root" (required string) ~doc:"PATH" in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind filenames = Reader.file_contents filelist in
        let filenames =
          List.map (String.split_lines filenames)
            ~f:(fun a -> filelist_root ^/ a)
        in
        let%bind features =
          Deferred.List.map filenames ~how:(`Max_concurrent_jobs 32)
            ~f:(fun filename ->
              let%bind reader = Reader.open_file filename in
              let%bind features = Reader.read_marshal reader in
              let%map () = Reader.close reader in
              let features =
                match features with
                | `Eof -> failwith "bla"
                | `Ok a -> a
              in
              features)
        in
        let features = List.concat features in
        let (keys : string list option ref) = ref None in
        Deferred.List.iter ~how:`Sequential features ~f:(fun row ->
          let row = Feature_extractor.to_csv_row row in
          begin match !keys with
          | None ->
            keys := Some (String.Map.keys row);
            printf "%s\n" (String.concat ~sep:"," (String.Map.keys row))
          | Some keys ->
            let equal = String.equal in
            assert (List.equal keys (String.Map.keys row) ~equal)
          end;
          printf "%s\n" (String.concat ~sep:"," (String.Map.data row));
          Deferred.unit
        )
    ]
  |> Command.run
