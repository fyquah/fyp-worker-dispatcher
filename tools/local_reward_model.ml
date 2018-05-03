open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils

let wait sec = Clock.after (Time.Span.of_sec sec)

module Absolute_path = Protocol.Absolute_path

module Raw_reward = struct
  type dual_reward =
    { immediate : float;
      long_term : float;
    }
  [@@deriving sexp]

  type t =
    { path             : Protocol.Absolute_path.t;
      inline_reward    : dual_reward option;
      no_inline_reward : float option
    }
  [@@deriving sexp, fields]
end

module Specification_file = struct
  type entry =
    { features_file : string;
      rewards_file  : string;
      name          : string;
    }
  [@@deriving sexp]

  type t = entry list [@@deriving sexp]
end

module Epoch_snapshot = struct
  type entry = {
    accuracy : float;
    loss : float;
  }
  [@@deriving sexp]

  type t = {
    epoch      : int;
    training   : entry option;
    validation : entry option;
    test       : entry option;
  }
  [@@deriving sexp]
end


let load_call_site_examples (specification: Specification_file.t) =
  Deferred.List.concat_map specification ~f:(fun specification_entry ->
      let features_file = specification_entry.features_file in
      let rewards_file = specification_entry.rewards_file in
      let%bind (features : Feature_extractor.t list) =
        Reader.with_file features_file ~f:(fun rdr ->
          Reader.read_marshal rdr >>= function
          | `Eof -> failwith "Cannot read somethign like this"
          | `Ok value -> return value)
      in
      let%map (raw_rewards : Raw_reward.t list) =
        Reader.load_sexp_exn rewards_file [%of_sexp: Raw_reward.t list]
        >>| List.map ~f:(fun (reward : Raw_reward.t) ->
            { reward with path = Absolute_path.compress reward.path })
      in
      let rewards =
        List.map raw_rewards ~f:(fun entry ->
          (Raw_reward.path entry,
          (Raw_reward.inline_reward entry, Raw_reward.no_inline_reward entry)))
        |> Protocol.Absolute_path.Map.of_alist_exn
      in
      let examples =
        List.filter_map features ~f:(fun feature_entry ->
            let trace =
              Protocol.Absolute_path.of_trace feature_entry.trace
              |> Absolute_path.compress
            in
            Option.map (Absolute_path.Map.find rewards trace)
              ~f:(fun r -> (feature_entry, r)))
        in
      Log.Global.info "%s | Loaded %d reward entries"
        specification_entry.name (Absolute_path.Map.length rewards);
      Log.Global.info "%s | Loaded %d feature entries"
        specification_entry.name (List.length features);
      Log.Global.info "%s | Loaded %d training examples"
        specification_entry.name (List.length examples);
      examples)
  >>| fun examples ->
  Log.Global.info "Loaded a total of %d training examples"
    (List.length examples);
  List.permute examples
;;

let shape_to_string (a, b) = sprintf "(%d, %d)" a b

module Familiarity_model = struct

  open Tensorflow_fnn

  module Neural = Owl.Neural.D

  module Classification_data = struct
    type t =
      { features : Owl.Mat.mat;
        labels   : int array;
        targets  : Owl.Mat.mat;
      }
  end

  type tf_model = 
    { model : (Tensorflow_fnn.Fnn._1d, [ `float ], Tensorflow_core.Tensor.float32_elt) Tensorflow_fnn.Fnn.Model.t;
      input_id: Tensorflow_fnn.Fnn.Input_id.t
    }

  type model = 
    { tf_model                         : tf_model;
      create_normalised_feature_vector : Feature_extractor.t -> Owl.Mat.mat;
      num_classes                      : int;
      training                         : Classification_data.t;
    }

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

  type target = (Raw_reward.dual_reward option * float option)
  type example = (Feature_extractor.t * target)

  let create_label_from_target ((a, b) : target) =
    if Option.is_some a && Option.is_some b then begin
      1
    end else begin
      0
    end
  ;;

  let create_model (examples: example list) =
    let raw_features = Array.of_list_map examples ~f:fst in
    let raw_targets  = Array.of_list_map examples ~f:snd in
    let create_normalised_feature_vector =
      Staged.unstage (
        Feature_engineering.create_feature_transformer
          (Array.to_list raw_features)
      )
    in
    let num_classes = 2 in
    let feature_matrix =
      List.map examples ~f:fst
      |> Array.of_list_map ~f:create_normalised_feature_vector
      |> Owl.Mat.concatenate ~axis:0
    in
    let num_features = Owl.Mat.col_num feature_matrix in
    let labels = Array.map raw_targets ~f:create_label_from_target in
    let target_matrix = target_matrix_of_labels ~num_classes:2 labels in
    let tf_model =
      let open Tensorflow_fnn in
      let input, input_id = Fnn.input ~shape:(D1 num_features) in
      let model =
        input
        |> Fnn.dense 32 ~w_init:(`normal 0.1)
        |> Fnn.relu
        |> Fnn.dense 16 ~w_init:(`normal 0.1)
        |> Fnn.relu
        |> Fnn.dense 2 ~w_init:(`normal 0.1)
        |> Fnn.softmax
        |> Fnn.Model.create Float
      in
      { input_id; model; }
    in
    { tf_model;
      create_normalised_feature_vector;
      num_classes;
      training = {
        Classification_data.
        features = feature_matrix; labels; targets = target_matrix
      };
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

  let gen_snapshot_entry model (data: Classification_data.t) =
    let loss_fn target_matrix probabilities =
      let open Owl.Optimise.D in
      let num_examples = Owl.Mat.row_num target_matrix in
      Loss.run Loss.Cross_entropy (Arr target_matrix) (Arr probabilities) 
      |> unpack_flt
      |> fun x -> x /. (Float.of_int num_examples) /. 2.0
    in
    let probabilities =
      let tf_model = model.tf_model in
      let tensor =
        Tensorflow_core.Tensor.of_float_array2
          (Owl.Mat.to_arrays data.features) Bigarray.Float32
      in
      Fnn.Model.predict model.tf_model.model
        [(tf_model.input_id, tensor)]
      |> Tensorflow_core.Tensor.to_float_array2
      |> Owl.Mat.of_arrays
    in
    let loss = loss_fn data.targets probabilities in
    let accuracy =
      guesses_of_probabilities probabilities
      |> compute_accuracy ~labels:data.labels
    in
    { Epoch_snapshot. loss; accuracy; }
  ;;

  let train_model model
      ~epochs
      ~(test_data: Classification_data.t)
      ~(validation_data: Classification_data.t) =
    let chkpt =
      let streak = ref 0 in
      let prev_accuracy = ref 0.0 in
      fun (state : Owl.Optimise.D.Checkpoint.state) ->
        let epoch = state.current_batch / state.batches_per_epoch in
        let validation_snapshot = gen_snapshot_entry model validation_data in
        let msg =
          let validation = Some validation_snapshot in
          let training = Some (gen_snapshot_entry model model.training) in
          let test = Some (gen_snapshot_entry model test_data) in
          { Epoch_snapshot. epoch; training; validation; test; }
          |> Epoch_snapshot.sexp_of_t
        in
        Owl_log.info "%s" (Sexp.to_string_mach msg);
        if validation_snapshot.accuracy <. !prev_accuracy then begin
          streak := !streak + 1
        end else begin
          streak := 0
        end;
        if !streak > 5 then begin
          state.stop <- true
        end;
        prev_accuracy := validation_snapshot.accuracy
    in
    let module Neural = Owl.Neural.D in
    let checkpoint =
      let feature_matrix = model.training.features in
      let target_matrix = model.training.targets in
      Fnn.Model.fit model.tf_model.model
        ~loss:(Fnn.Loss.cross_entropy `mean)
        ~optimizer:(Fnn.Optimizer.adam ~learning_rate:0.0001 ~beta1:0.001 ~beta2:0.0001 ())
        ~epochs
        ~input_id:model.tf_model.input_id
        ~xs:(tensor_of_mat feature_matrix)
        ~ys:(tensor_of_mat target_matrix);
    in
    let baseline =
      let to_mat labels =
        Owl.Mat.of_array (Array.map ~f:Float.of_int labels)
          (Array.length labels) 1
      in
      Owl.Mat.mean' (to_mat model.training.labels)
    in
    let ss =
      let training = Some (gen_snapshot_entry model model.training) in
      let validation = Some (gen_snapshot_entry model validation_data) in
      let test = Some (gen_snapshot_entry model test_data) in
      { Epoch_snapshot. epoch = epochs; training; validation; test; }
    in
    Log.Global.sexp ~level:`Info (Epoch_snapshot.sexp_of_t ss);
    checkpoint
  ;;

  let do_analysis ~epochs (examples : example list) =
    let training_examples, validation_examples, test_examples =
      let num_training_examples =
        Float.(to_int (0.7 *. of_int (List.length examples)))
      in
      let train, rest = List.split_n examples num_training_examples in
      let validation, test = List.split_n rest ((List.length rest) / 2) in
      (train, validation, test)
    in
    let model = create_model training_examples in
    let generate_features_and_labels examples =
      let features =
        List.map ~f:fst examples
        |> List.map ~f:model.create_normalised_feature_vector
        |> List.to_array
        |> Owl.Mat.concatenate ~axis:0
      in
      let labels =
        List.map ~f:snd examples
        |> List.map ~f:create_label_from_target
        |> List.to_array
      in
      let targets = target_matrix_of_labels ~num_classes:2 labels in
      { Classification_data. features; labels; targets; }
    in
    let test_data = generate_features_and_labels test_examples in
    let validation_data =
      generate_features_and_labels validation_examples
    in
    let _checkpoint =
      train_model ~epochs ~validation_data ~test_data model
    in
    let test_ss = gen_snapshot_entry model test_data in
    Deferred.return ()
  ;;
    
  let command =
    let open Command.Let_syntax in
    Command.async ~summary:"Linear reward model"
      [%map_open
        let specification_file =
          flag "-spec" (required file) ~doc:"FILE specification file"
        and epochs =
          flag "-epochs" (required int) ~doc:"INT epochs"
        in
        fun () ->
          let open Deferred.Let_syntax in
          let%bind specification =
            Reader.load_sexp_exn specification_file
              Specification_file.t_of_sexp
          in
          let%bind examples = load_call_site_examples specification in
          let%bind () = wait 0.1 in

          (* Real analysis begins here. *)
          do_analysis ~epochs examples
      ]
  ;;
end

let () =
  Command.group ~summary:"Local reward model" [
    ("famliarity-model", Familiarity_model.command)
  ]
  |> Command.run
;;

(*
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
  ;;
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
*)
