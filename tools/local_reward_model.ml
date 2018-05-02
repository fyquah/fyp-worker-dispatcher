open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Absolute_path = Protocol.Absolute_path


module Raw_reward = struct
  type t = Protocol.Absolute_path.t * float option * float option
  [@@deriving sexp]
end

module Reward = struct
  type t = Protocol.Absolute_path.t * float * float
  [@@deriving sexp]
end

module Linear_reward_model = struct

  module Neural = Owl.Neural.D

  type model = 
    { network                          : Neural.Graph.network;
      create_normalised_feature_vector : Feature_extractor.t -> Owl.Mat.mat;
      create_label                     : (float option * float option) -> int;
      num_classes                      : int;
      training_feature_matrix          : Owl.Mat.mat;
      training_targets                 : Owl.Mat.mat;
    }

  let fst4 (a, _, _, _) = a

  let snd4 (_, a, _, _) = a

  let third4 (_, _, a, _) = a

  let forth4 (_, _, _, a) = a

  let shape_to_string (a, b) = sprintf "(%d, %d)" a b

  let discretise_int_feature ~lo ~hi x =
    let ret = Array.create ~len:(hi - lo + 1 + 2) false in
    assert (lo < hi);
    if x < lo then
      ret.(0) <- true
    else if x > hi then
      ret.(Array.length ret - 1) <- true
    else
      ret.(x - lo + 1) <- true;
    ret
  ;;

  let guesses_of_probabilities probabilities =
    let a = Owl.Mat.max_rows probabilities in
    Array.map a ~f:(fun (_, _r, c) ->
      assert (c = 0 || c = 1);
      c)
  ;;

  let target_matrix_of_labels ~num_classes labels = 
    let mat = Owl.Mat.create (Array.length labels) num_classes 0.0 in
    for i = 0 to Array.length labels - 1 do
      Owl.Mat.set mat i labels.(i) 1.0;
    done;
    mat
  ;;

  let is_small x = Float.(abs x <= 0.00001)

  let create_model (examples: (Feature_extractor.t * (float option * float option)) list) =
    let create_features (feature_vector : Feature_extractor.t) =
      let int_features = [|
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

        feature_vector.flambda_wsb.original_size;
        feature_vector.flambda_wsb.new_size;
        feature_vector.flambda_wsb.benefit_remove_call;
        feature_vector.flambda_wsb.benefit_remove_alloc;
        feature_vector.flambda_wsb.benefit_remove_prim;
        feature_vector.flambda_wsb.benefit_remove_branch;
        feature_vector.flambda_wsb.benefit_direct_call_of_indirect;
      |]
      in
      let simple_boolean_features = [|
        feature_vector.is_annonymous;
        feature_vector.is_a_functor;
        feature_vector.direct_call;
        feature_vector.recursive_call;
        feature_vector.is_recursive;
        feature_vector.only_use_of_function;
        feature_vector.in_recursive_function;
        feature_vector.flambda_wsb.toplevel;
        feature_vector.flambda_wsb.lifting;
      |]
      in
      let boolean_features =
        Array.concat [
          simple_boolean_features;
          discretise_int_feature ~lo:0 ~hi:8 feature_vector.flambda_wsb.branch_depth;
          discretise_int_feature ~lo:0 ~hi:8 feature_vector.inlining_depth;
          discretise_int_feature ~lo:0 ~hi:8 feature_vector.closure_depth;
        ]
      in
      (int_features, boolean_features)
    in
    let processed_features =
      List.map examples
        ~f:(fun (feature_vector, (reward_inline, reward_no_inline)) ->
          let int_features, boolean_features =
            create_features feature_vector 
          in
          (int_features, boolean_features, reward_inline, reward_no_inline))
      |> Array.of_list
    in
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
      |> (fun m ->
          if Mat.col_num m > 0 then
            Owl.Mat.mean_rows m
          else
            m)
      |> Owl.Mat.to_array
    in
    let sqr x = x *. x in
    let feature_std =
      Array.map ~f:fst4 processed_features
      |> Array.map ~f:(Array.mapi ~f:(fun j x ->
          sqr (Float.of_int x -. feature_means.(j))))
      |> Owl.Mat.of_arrays
      |> (fun m -> 
          if Mat.col_num m > 0 then
            Owl.Mat.mean_rows m
          else
            m)
      |> Owl.Mat.sqrt
      |> Owl.Mat.to_array
    in
    assert (Array.length feature_std   = num_int_features);
    assert (Array.length feature_means = num_int_features);
    let create_normalised_feature_vector =
      fun feature_set ->
        let (int_features, boolean_features) = create_features feature_set in
        let feature_vector = Mat.create 1 num_features 0.0 in
        for j = 0 to num_int_features - 1 do
          let value = (Float.of_int int_features.(j)) in
          let value =
            if is_small feature_std.(j) then
              value -. feature_means.(j)
            else
              (value -. feature_means.(j)) /. feature_std.(j)
          in
          Mat.set feature_vector 0 j value
        done;
        for j = 0 to num_bool_features - 1 do
          let flag = boolean_features.(j) in
          let value = if flag then 1.0 else 0.0 in
          Mat.set feature_vector 0 (j + num_int_features) value
        done;
        feature_vector
    in
    let create_label ((a : float option), (b : float option)) =
      if Option.is_some a && Option.is_some b then begin
        1
      end else begin
        0
      end
    in
    let num_classes = 2 in
    let feature_matrix =
      List.map examples ~f:fst
      |> List.map ~f:create_normalised_feature_vector
      |> List.to_array
      |> Owl.Mat.concatenate ~axis:0
    in
    let labels =
      let ret = Array.create ~len:num_examples 0 in
      for i = 0 to num_examples - 1 do
        ret.(i) <- create_label
            (third4 processed_features.(i), forth4 processed_features.(i))
      done;
      ret
    in
    let target_matrix = target_matrix_of_labels ~num_classes:2 labels in
    let network =
      let open Owl.Neural.D in
      let open Owl.Neural.D.Graph in
      input [| num_features |]
      |> fully_connected 32 ~init_typ:Init.LecunNormal
          ~act_typ:Activation.Relu
      |> fully_connected 16 ~init_typ:Init.LecunNormal
          ~act_typ:Activation.Relu
      |> linear 2 ~init_typ:Init.LecunNormal
          ~act_typ:Activation.Softmax
      |> get_network
    in
    { network;
      create_normalised_feature_vector;
      create_label;
      num_classes;
      training_feature_matrix = feature_matrix;
      training_targets = target_matrix;
    }
  ;;

  let compute_accuracy ~labels guesses =
    assert (Array.length labels = Array.length guesses);
    let to_mat labels =
      Owl.Mat.of_array (Array.map ~f:Float.of_int labels)
        (Array.length labels) 1
    in
    Owl.Mat.(mean' (to_mat guesses =. to_mat labels))
  ;;

  let train_model ?state ~epochs model =
    let params =
      let open Owl.Optimise.D in
      Owl.Optimise.D.Params.config
        ~batch:Batch.Full
        ~gradient:Gradient.GD
        ~learning_rate:(Learning_Rate.Adagrad 0.01)
        ~loss:Loss.Cross_entropy
        ~verbosity:false
        ~stopping:Stopping.None
        (Float.of_int epochs)
    in
    let module Neural = Owl.Neural.D in
    let network = model.network in
    let feature_matrix = model.training_feature_matrix in
    let target_matrix = model.training_targets in
    let num_examples = Owl.Mat.row_num feature_matrix in
    let checkpoint =
      Neural.Graph.train ?state ~params network feature_matrix
        model.training_targets
    in
    let model_fn = Owl.Neural.D.Graph.model network in
    let open Owl.Optimise.D in
    let probabilities = model_fn feature_matrix in
    let loss =
      let open Owl.Optimise.D in
      Loss.run Loss.Cross_entropy (Arr target_matrix) (Arr probabilities) 
      |> unpack_flt
      |> fun x -> x /. (Float.of_int num_examples)
    in
    let guesses = guesses_of_probabilities probabilities in
    let labels = guesses_of_probabilities target_matrix in
    let accuracy = compute_accuracy ~labels guesses in
    let baseline =
      let to_mat labels =
        Owl.Mat.of_array (Array.map ~f:Float.of_int labels)
          num_examples 1
      in
      Owl.Mat.mean' (to_mat labels)
    in
    Log.Global.info "Training baseline accuracy = %f" baseline;
    Log.Global.info "training set loss = %f" loss;
    Log.Global.info "training epochs = %f" checkpoint.epochs;
    Log.Global.info "training stopped = %b" checkpoint.stop;
    Log.Global.info "training set accuracy = %f" accuracy;
    checkpoint
  ;;

  let do_analysis examples =
    let training_examples, validation_examples, test_examples =
      let num_training_examples =
        Float.(to_int (0.7 *. of_int (List.length examples)))
      in
      let train, rest = List.split_n examples num_training_examples in
      let validation, test = List.split_n rest ((List.length rest) / 2) in
      (train, validation, test)
    in
    let model = create_model training_examples in
    let%bind checkpoint =
      let validation_features =
        List.map ~f:fst validation_examples
        |> List.map ~f:model.create_normalised_feature_vector
        |> List.to_array
        |> Owl.Mat.concatenate ~axis:0
      in
      let validation_labels =
        List.map ~f:snd validation_examples
        |> List.map ~f:model.create_label
        |> List.to_array
      in
      let best_so_far = ref None in
      let rec loop ~iter ~prev =
        Log.Global.info ">>>>> Iteration %d <<<<<" iter;
        let checkpoint =
          let epochs = (iter + 1) * 100 in
          train_model ?state:(Option.map ~f:fst prev) ~epochs model
        in
        let accuracy =
          let validation_probabilities =
            Neural.Graph.model model.network validation_features
          in
          let validation_guesses =
            guesses_of_probabilities validation_probabilities
          in
          compute_accuracy ~labels:validation_labels validation_guesses
        in
        let () =
          match !best_so_far with
          | None -> 
            best_so_far := Some (model, accuracy)
          | Some (_, last_best_accuracy)->
            if accuracy >. last_best_accuracy then begin
              best_so_far := Some (model, accuracy)
            end
        in
        Log.Global.info "validation accuracy = %f" accuracy;
        match prev with
        | Some (_checkpoint, prev_accuracy)
          when (prev_accuracy > accuracy || iter < 200) && iter > 100 ->
          Deferred.unit
        | _ ->
          Clock.after (Time.Span.of_sec 0.01) >>= fun () ->
          loop ~prev:(Some (checkpoint, accuracy)) ~iter:(iter + 1)
      in
      let%bind() = loop ~prev:None ~iter:0 in
      Deferred.return (Option.value_exn !best_so_far)
    in
    let model_fn = Neural.Graph.model model.network in
    let test_features =
      List.map ~f:fst test_examples
      |> List.map ~f:model.create_normalised_feature_vector
      |> List.to_array
      |> Owl.Mat.concatenate ~axis:0
    in
    let test_probabilities = model_fn test_features in
    let test_guesses = guesses_of_probabilities test_probabilities in
    let test_labels =
      List.map ~f:snd test_examples
      |> List.map ~f:model.create_label
      |> List.to_array
    in
    let test_accuracy =
      let to_mat labels =
        Owl.Mat.of_array (Array.map ~f:Float.of_int labels)
          (Owl.Mat.row_num test_features) 1
      in
      Owl.Mat.(mean' (to_mat test_guesses =. to_mat test_labels))
    in
    Log.Global.info "Test accuracy: %f" test_accuracy;
    Deferred.return ()
  ;;
    
  let command =
    let open Command.Let_syntax in
    Command.async ~summary:"Linear reward model"
      [%map_open
        let features_file =
          flag "-features" (required file) ~doc:"FILE features bin file"
        and rewards_file =
          flag "-rewards" (required file) ~doc:"FILE raw rewards sexp file"
        in
        fun () ->
          let open Deferred.Let_syntax in
          let%bind (features : Feature_extractor.t list) =
            Reader.with_file features_file ~f:(fun rdr ->
              Reader.read_marshal rdr >>= function
              | `Eof -> failwith "Cannot read somethign like this"
              | `Ok value -> return value)
          in
          let%bind (raw_rewards : Raw_reward.t list) =
            Reader.load_sexp_exn rewards_file [%of_sexp: Raw_reward.t list]
            >>| List.map ~f:(fun (trace, a, b) -> (Absolute_path.compress trace, a, b))
          in
          let reward_traces =
            List.map raw_rewards ~f:(fun (trace, _, _) -> trace)
          in
          let feature_traces =
            List.map features ~f:(fun feature ->
                Absolute_path.compress (Absolute_path.of_trace feature.trace))
          in
          let print_traces buffer traces =
            List.map ~f:Absolute_path.sexp_of_t traces
            |> List.sort ~compare:Sexp.compare
            |> List.iter ~f:(fun trace ->
                bprintf buffer "%s\n" (Sexp.to_string_hum trace);
                bprintf buffer  ">>>>>>>>>>>>>>>>>>\n";
              );
          in
          let rewards =
            List.map raw_rewards ~f:(fun (a, b, c) -> (Absolute_path.compress a, (b, c)))
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
