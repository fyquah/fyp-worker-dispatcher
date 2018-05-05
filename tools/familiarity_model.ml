open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils

open Tensorflow_fnn

module Classification_data = struct
  type t =
    { features : Owl.Mat.mat;
      labels   : int array;
      targets  : Owl.Mat.mat;
    }
end

let wait sec = Clock.after (Time.Span.of_sec sec)

module Epoch_snapshot = struct
  type entry = {
    accuracy : float;
    loss     : float;
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


module O = Tensorflow.Ops

type node = [ `float ] Tensorflow.Node.t
type placeholder = [`float] O.Placeholder.t

type tf_model = 
  { input_placeholder       : placeholder;
    target_placeholder      : placeholder;
    is_training_placeholder : [ `int32 ] O.Placeholder.t;
    output                  : node;
    vars                    : node list;
    loss                    : node;
    gd                      : Tensorflow.Node.p list;
  }

type hyperparams =
  { l2_reg  : float sexp_option;
    dropout_keep_prob : float sexp_option;
  }
[@@deriving sexp]

let construct_tf_model ~hyperparams num_features =
  let open Tensorflow in
  let vars = ref [] in
  let weights = ref [] in
  let linear num_in num_out input =
    let hi = (1.0 /. Float.(sqrt (of_int num_in * of_int num_out))) in
    let lo = -.hi in
    let w = Var.uniformf ~lo ~hi [ num_in; num_out ] in
    let b = Var.f [ num_out ] 0. in
    vars := (w :: b :: !vars);
    weights := (w :: !weights);
    O.(input *^ w + b)
  in
  let input_placeholder   =
    O.placeholder [ -1; num_features; ] ~type_:Float
  in
  let target_placeholder =
    O.placeholder [ -1; 2; ] ~type_:Float
  in
  let is_training_placeholder = O.placeholder [] ~type_:Int32 in
  let is_training =
    O.notEqual (O.const_int ~shape:[] ~type_:Int32 [0])
      (O.Placeholder.to_node is_training_placeholder)
  in
  let dropout_prob p =
    O.cond is_training ~if_true:(O.f p) ~if_false:(O.f 1.0)
  in
  let maybe_dropout node =
    match hyperparams.dropout_keep_prob with
    | None -> node
    | Some p -> O.dropout ~keep_prob:(dropout_prob p) node
  in
  let output =
    O.Placeholder.to_node input_placeholder
    |> linear num_features 32
    |> O.relu
    |> maybe_dropout
    |> linear 32 16
    |> O.relu
    |> maybe_dropout
    |> linear 16 2
    |> O.softmax
  in
  let loss =
    let ce =
      O.cross_entropy ~ys:(O.Placeholder.to_node target_placeholder)
        ~y_hats:output `mean
    in
    let l2_reg_term =
      let weights_squared =
        List.map ~f:(fun a -> O.reduce_sum (O.mul a a)) !weights
        |> List.fold ~f:O.add ~init:(O.f 0.0)
      in
      match hyperparams.l2_reg with
      | Some lambda -> O.(f lambda * weights_squared)
      | None -> O.f 0.0
    in
    O.add ce l2_reg_term
  in
  let vars = !vars in
  let gd = Optimizers.adam_minimizer ~learning_rate:(O.f 0.01) loss in
  { is_training_placeholder;
    input_placeholder; target_placeholder; output; vars; loss; gd; }
;;

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

let create_model ~hyperparams (examples: example list) =
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
  let tf_model = construct_tf_model ~hyperparams num_features in
  { tf_model;
    create_normalised_feature_vector;
    num_classes;
    training = {
      Classification_data.
      features = feature_matrix; labels; targets = target_matrix
    };
  }
;;

type prediction =
  { probabilities: Owl.Mat.mat;
    loss : float;
  }

let predict (tf_model : tf_model) ~session
    ~(target_matrix : Owl.Mat.mat) (feature_matrix : Owl.Mat.mat) =
  let open Tensorflow in
  let feature_matrix = tensor_of_mat feature_matrix in
  let target_matrix = tensor_of_mat target_matrix in
  let session_inputs = Session.Input.[
    float tf_model.input_placeholder feature_matrix;
    float tf_model.target_placeholder target_matrix;
    int32 tf_model.is_training_placeholder (tensor_scalar_int32 0);
  ]
  in
  let probabilities, loss =
    Session.run ~session ~inputs:session_inputs
      (Session.Output.(both
        (float tf_model.output) (scalar_float tf_model.loss)))
  in
  let probabilities = tensor_to_mat probabilities in
  { probabilities; loss; }
;;

let compute_accuracy ~labels guesses =
  assert (Array.length labels = Array.length guesses);
  let to_mat labels =
    Owl.Mat.of_array (Array.map ~f:Float.of_int labels)
      (Array.length labels) 1
  in
  Owl.Mat.(mean' (to_mat guesses =. to_mat labels))
;;

let gen_snapshot_entry ~session model (data: Classification_data.t) =
  let probabilities, loss =
    let tf_model = model.tf_model in
    let { probabilities; loss } =
      predict tf_model ~session ~target_matrix:data.targets data.features
    in
    (probabilities, loss)
  in
  let accuracy =
    guesses_of_probabilities probabilities
    |> compute_accuracy ~labels:data.labels
  in
  { Epoch_snapshot. loss; accuracy; }
;;

let train_model model
    ~epochs:total_epochs
    ~(test_data: Classification_data.t)
    ~(validation_data: Classification_data.t) =
  let session =
    let options =
      let config =
        Tensorflow_core.Protobuf.read_file "tf_config.pb"
      in
      let open Tensorflow.Session.Options in
      add_config empty config
    in
    Tensorflow.Session.create ~options ()
  in
  let check_stopping_cond =
    let streak = ref 0 in
    let prev_loss = ref 0.0 in
    fun () ->
      let validation_snapshot =
        gen_snapshot_entry ~session model validation_data
      in
      if validation_snapshot.loss <. !prev_loss then begin
        streak := !streak + 1
      end else begin
        streak := 0
      end;
      prev_loss := validation_snapshot.loss;
      if !streak > 20 then begin
        `Stop
      end else begin
        `Continue
      end
  in
  let gen_snapshot ~session model epoch =
    let ss =
      let training = Some (gen_snapshot_entry ~session model model.training) in
      let validation = Some (gen_snapshot_entry ~session model validation_data) in
      let test = Some (gen_snapshot_entry ~session model test_data) in
      { Epoch_snapshot. epoch; training; validation; test; }
    in
    ss
  in
  let print_snapshot ss =
    Log.Global.sexp ~level:`Info (Epoch_snapshot.sexp_of_t ss);
  in
  let%bind () =
    let open Tensorflow in
    let tf_model = model.tf_model in
    let feature_matrix = tensor_of_mat model.training.features in
    let target_matrix  = tensor_of_mat model.training.targets in

    Deferred.repeat_until_finished 0 (fun current_epoch ->
      Session.run ~session ~targets:tf_model.gd ~inputs:Session.Input.[
        float tf_model.input_placeholder feature_matrix;
        float tf_model.target_placeholder target_matrix;
        int32 tf_model.is_training_placeholder (tensor_scalar_int32 1);
      ] Session.Output.empty;

      (* Check if it is time to stop *)
      let continue =
        if current_epoch < 100 || not (current_epoch mod 10 = 0) then
          true
        else begin match check_stopping_cond () with
        | `Stop -> false
        | `Continue -> true
        end
      in
      (* prepare for the next epoch *)
      print_snapshot (gen_snapshot ~session model current_epoch);
      Writer.flushed (Lazy.force Writer.stderr)
      >>= fun () ->

      if continue && current_epoch < total_epochs then
        Deferred.return (`Repeat (current_epoch + 1))
      else
        Deferred.return (`Finished ()))
  in
  let baseline =
    let to_mat labels =
      Owl.Mat.of_array (Array.map ~f:Float.of_int labels)
        (Array.length labels) 1
    in
    Owl.Mat.mean' (to_mat test_data.labels)
  in
  let baseline_accuracy = Float.max baseline (1.0 -. baseline) in
  Log.Global.info "Baseline test accuracy = %f" baseline_accuracy;
  Deferred.unit
;;

let do_analysis ~hyperparams ~epochs ~(test_examples : example list) 
    (examples : example list) =
  let training_examples, validation_examples =
    let num_training_examples =
      Float.(to_int (0.8 *. of_int (List.length examples)))
    in
    List.split_n examples num_training_examples
  in
  let model = create_model ~hyperparams training_examples in
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
  let%bind () =
    train_model ~epochs ~validation_data ~test_data model
  in
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
      and hyperparams_file =
        flag "-hyperparams" (required file) ~doc:"FILE hyperparams file"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind specification =
          Reader.load_sexp_exn specification_file
            Specification_file.t_of_sexp
        in
        let%bind hyperparams =
          Reader.load_sexp_exn hyperparams_file [%of_sexp: hyperparams]
        in
        let%bind training_examples, test_examples =
          match specification with
          | Specification_file.Segmented { training; test; } -> 
            Deferred.both
              (load_call_site_examples training)
              (load_call_site_examples test)
            >>= fun (training, test) ->
            Log.Global.info
              "Loaded %d IN-SAMPLE training examples and \
               %d OUT-OF-SAMPLE test examples"
              (List.length training)
              (List.length test);
            return (training, test)
          | Specification_file.Unsegmented entries -> 
            load_call_site_examples entries
            >>| fun examples ->
            let n = List.length examples * 7 / 10 in
            List.split_n examples n
        in
        Log.Global.sexp ~level:`Info [%message (hyperparams: hyperparams)];
        let%bind () = wait 0.1 in

        (* Real analysis begins here. *)
        do_analysis ~hyperparams ~epochs ~test_examples training_examples
    ]
;;
