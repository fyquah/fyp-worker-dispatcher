open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Absolute_path = Protocol.Absolute_path

module Reward = struct
  type t = Protocol.Absolute_path.t * float * float
  [@@deriving sexp]
end

module Linear_reward_model = struct
  let do_analysis (examples: (Feature_extractor.t * (float * float)) list) =
    let processed_features =
      List.map examples
        ~f:(fun (feature_vector, (reward_inline, reward_no_inline)) ->
          (* feature_vector.expected_allocations             ; *)
          let int_features = [
            feature_vector.params;
            feature_vector.params                           ;
            feature_vector.bound_vars_to_symbol             ;
            feature_vector.assign                           ;
            feature_vector.bound_vars_to_mutable            ;
            feature_vector.bound_vars                       ;
            feature_vector.free_vars                        ;
            feature_vector.free_symbols                     ;
            feature_vector.set_of_closures                  ;
            feature_vector.non_specialized_args             ;
            feature_vector.specialized_args                 ;
            feature_vector.size_before_simplify             ;
            feature_vector.size_after_simplify              ;
            feature_vector.underlying_direct_applications   ;
            feature_vector.underlying_indirect_applications ;
            feature_vector.if_then_else                     ;
            feature_vector.switch                           ;
            feature_vector.string_switch                    ;
            feature_vector.inlining_depth;
            feature_vector.closure_depth;

            feature_vector.flambda_wsb.branch_depth;
            feature_vector.flambda_wsb.original_size;
            feature_vector.flambda_wsb.new_size;
            feature_vector.flambda_wsb.benefit_remove_call;
            feature_vector.flambda_wsb.benefit_remove_alloc;
            feature_vector.flambda_wsb.benefit_remove_prim;
            feature_vector.flambda_wsb.benefit_remove_branch;
            feature_vector.flambda_wsb.benefit_direct_call_of_indirect;
          ]
          in
          let boolean_features = [
            feature_vector.is_annonymous;
            feature_vector.is_a_functor;
            feature_vector.direct_call;
            feature_vector.recursive_call;
            feature_vector.is_recursive;
            feature_vector.only_use_of_function;
            feature_vector.in_recursive_function;
            feature_vector.flambda_wsb.toplevel;
            feature_vector.flambda_wsb.lifting;
          ]
          in
          (Array.of_list int_features,
           Array.of_list boolean_features,
           reward_inline,
           reward_no_inline))
      |> Array.of_list
    in
    let fst4 (a, _, _, _) = a in
    let snd4 (_, a, _, _) = a in
    let third4 (_, _, a, _) = a in
    let forth4 (_, _, _, a) = a in
    let num_examples = Array.length processed_features in
    let num_int_features =
      let (a, _, _, _) = processed_features.(0) in
      Array.length a
    in
    let num_bool_features =
      let (_, b, _, _) = processed_features.(0) in
      Array.length b
    in
    let num_features = num_int_features + num_bool_features in
    let module Mat = Owl.Mat in

    let feature_means =
      Array.map ~f:fst4 processed_features
      |> Array.map ~f:(Array.map ~f:Float.of_int)
      |> Owl.Mat.of_arrays
      |> Owl.Mat.average_rows
      |> Owl.Mat.to_array
    in
    let sqr x = x *. x in
    let feature_std =
      Array.map ~f:fst4 processed_features
      |> Array.map ~f:(Array.mapi ~f:(fun j x -> sqr (Float.of_int x -. feature_means.(j))))
      |> Owl.Mat.of_arrays
      |> Owl.Mat.average_rows
      |> Owl.Mat.sqrt
      |> Owl.Mat.to_array
    in
    assert (Array.length feature_std   = num_int_features);
    assert (Array.length feature_means = num_int_features);
    let feature_normalisation = Array.zip_exn feature_means feature_std in

    let feature_matrix = Mat.create num_examples num_features 0.0 in
    let target_matrix = Mat.create num_examples 1 0.0 in
    let abs x = if x <. 0.0 then Float.neg x else x in
    let too_small x = abs x <= 0.000001 in

    (* Populate feature matrix *)
    for i = 0 to num_examples - 1 do
      for j = 0 to num_int_features - 1 do
        let value =
          (Float.of_int (fst4 processed_features.(i)).(j))
        in
        let value =
          if too_small feature_std.(j) then
            value
          else
            (value -. feature_means.(j)) /. feature_std.(j)
        in
        Mat.set feature_matrix i j value
      done;
      for j = 0 to num_bool_features - 1 do
        let flag = (snd4 processed_features.(i)).(j) in
        let value = if flag then 1.0 else 0.0 in
        Mat.set feature_matrix i (j + num_int_features) value
      done
    done;

    (* Populate target matrix *)
    for i = 0 to num_examples - 1 do
      let diff = 
        third4 processed_features.(i) -. forth4 processed_features.(i)
      in
      Mat.set target_matrix i 0 diff
    done;

    let obtained =
      Owl.Regression.D.ridge ~i:true ~a:0.0 feature_matrix target_matrix
    in
    let weights = obtained.(0) in
    let intercept = obtained.(1) in
    let shape_to_string (a, b) = sprintf "(%d, %d)" a b in
    printf !"Output: %{shape_to_string} %{shape_to_string} %{shape_to_string}\n"
      (Mat.shape feature_matrix) (Mat.shape weights) (Mat.shape intercept);

    let obtained_target =
      let repeated_intercept =
        Mat.repeat ~axis:0 intercept num_examples
      in
      printf !"Hello: %{shape_to_string}\n" (Mat.shape repeated_intercept);
      Mat.(feature_matrix *@ weights + intercept - target_matrix)
    in
    let mse = Mat.(average (sqr (obtained_target - target_matrix))) in
    printf "Mse: %.5f\n" mse;
    printf "Average value: %.5f\n" (Mat.average target_matrix);
    printf "Average absolute value: %.5f\n" (Mat.average (Mat.abs target_matrix));
    printf "Maximum error: %.5f\n" (Mat.max (Mat.(abs (obtained_target - target_matrix))));
    printf "Minimum error: %.5f\n" (Mat.min (Mat.(abs (obtained_target - target_matrix))));
    printf "Weights:\n";

    for i = 0 to num_features - 1 do
      printf " - (%d) %.5f\n" i (Mat.get weights i 0)
    done;
    Deferred.return ()
  ;;
    
  let command =
    let open Command.Let_syntax in
    Command.async' ~summary:"Linear reward model"
      [%map_open
        let features_file =
          flag "-features" (required file) ~doc:"FILE features bin file"
        and rewards_file =
          flag "-rewards" (required file) ~doc:"FILE rewards sexp file"
        in
        fun () ->
          let open Deferred.Let_syntax in
          let%bind (features : Feature_extractor.t list) =
            Reader.with_file features_file ~f:(fun rdr ->
              Reader.read_marshal rdr >>= function
              | `Eof -> failwith "Cannot read somethign like this"
              | `Ok value -> return value)
          in
          let%bind (rewards : Reward.t list) =
            Reader.load_sexp_exn rewards_file [%of_sexp: Reward.t list]
            >>| List.map ~f:(fun (trace, a, b) -> (Absolute_path.compress trace, a, b))
          in
          let reward_traces =
            List.map rewards ~f:(fun (trace, _, _) -> trace)
          in
          let feature_traces =
            List.map features ~f:(fun feature ->
                Absolute_path.compress (Absolute_path.of_trace feature.trace))
          in
          let print_traces buffer traces =
            List.map ~f:Absolute_path.sexp_of_t traces
            |> List.sort ~cmp:Sexp.compare
            |> List.iter ~f:(fun trace ->
                bprintf buffer "%s\n" (Sexp.to_string_hum trace);
                bprintf buffer  ">>>>>>>>>>>>>>>>>>\n";
              );
          in
          let rewards =
            List.map rewards ~f:(fun (a, b, c) -> (Absolute_path.compress a, (b, c)))
            |> Protocol.Absolute_path.Map.of_alist_exn
          in
          Log.Global.info "Loaded %d reward entries" (Absolute_path.Map.length rewards);
          Log.Global.info "Loaded %d feature entries" (List.length features);
          let examples =
            List.filter_map features ~f:(fun feature_entry ->
                let trace =
                  Protocol.Absolute_path.of_trace feature_entry.trace
                  |> Absolute_path.compress
                in
                Option.map (Absolute_path.Map.find rewards trace)
                  ~f:(fun r -> (feature_entry, r)))
          in
          Log.Global.info "Loaded %d training examples" (List.length examples);

          let _unused_debugging () =
            let%bind () =
              let buffer = Buffer.create 10 in
              print_traces buffer
                (List.map ~f:(fun a -> Absolute_path.of_trace (fst a).trace) examples);
              Writer.save "trace_from_examples.txt" ~contents:(Buffer.contents buffer)
            in
            let%bind () =
              let buffer = Buffer.create 10 in
              print_traces buffer reward_traces;
              Writer.save"trace_from_rewards.txt" ~contents:(Buffer.contents buffer)
            in
            let%bind () =
              let buffer = Buffer.create 10 in
              print_traces buffer feature_traces;
              Writer.save "trace_from_features.txt" ~contents:(Buffer.contents buffer)
            in
            Deferred.unit
          in

          (* Real analysis begins here. *)
          do_analysis examples
      ]
  ;;
end

let () =
  Command.group ~summary:"Local reward model" [
    ("linear", Linear_reward_model.command)
  ]
  |> Command.run
;;
