open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils

open Tensorflow_fnn


module Data : sig
  type classification =
    { features : Owl.Mat.mat;
      labels   : int array;
      targets  : Owl.Mat.mat;
    }

  type regression =
    { features : Owl.Mat.mat;
      targets  : Owl.Mat.mat;
    }

  type _ t =
    | Classification : classification -> [ `classification ] t
    | Regression     : regression     -> [ `regression ] t

  val features : _ t -> Owl.Mat.mat
  val targets  : _ t -> Owl.Mat.mat
  val labels      : [ `classification ] t -> int array
  val num_classes : [ `classification ] t -> int

end = struct
  type classification =
    { features : Owl.Mat.mat;
      labels   : int array;
      targets  : Owl.Mat.mat;
    }

  type regression =
    { features : Owl.Mat.mat;
      targets  : Owl.Mat.mat;
    }

  type _ t =
    | Classification : classification -> [ `classification ] t
    | Regression     : regression     -> [ `regression ] t

  let features (type a) (t : a t) = 
    match t with
    | Classification { features; _ } -> features
    | Regression { features; _ } -> features
  ;;

  let targets (type a) (t : a t) =
    match t with
    | Classification { targets; _ } -> targets
    | Regression { targets; _ } -> targets
  ;;

  let labels (t : [ `classification ] t) =
    match t with
    | Classification { labels; _ } -> labels

  let num_classes (t : [ `classification ] t) =
    match t with
    | Classification { targets; _ } -> Owl.Mat.col_num targets
end

module Epoch_snapshot = struct
  type entry = {
    accuracy : float sexp_option;
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

type 'a model = 
  { tf_model                         : tf_model;
    create_normalised_feature_vector : Feature_extractor.t -> Owl.Mat.mat;
    training                         : 'a Data.t;
  }

type prediction =
  { network_output: Owl.Mat.mat;
    loss : float;
  }

let guesses_of_probabilities probabilities =
  let a = Owl.Mat.max_rows probabilities in
  Array.map a ~f:(fun (_, _r, c) -> c)
;;

let is_small x = Float.(abs x <= 0.00001)

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
  let network_output = tensor_to_mat probabilities in
  { network_output; loss; }
;;

let compute_accuracy ~labels guesses =
  assert (Array.length labels = Array.length guesses);
  let to_mat labels =
    Owl.Mat.of_array (Array.map ~f:Float.of_int labels)
      (Array.length labels) 1
  in
  Owl.Mat.(mean' (to_mat guesses =. to_mat labels))
;;

let gen_snapshot_entry (type a) ~session (model: a model)
    (data: a Data.t) =
  let network_output, loss =
    let tf_model = model.tf_model in
    let { network_output; loss } =
      predict tf_model ~session ~target_matrix:(Data.targets data)
        (Data.features data)
    in
    (network_output, loss)
  in
  let accuracy =
    match data with
    | Data.Classification _ ->
      Some (
        guesses_of_probabilities network_output
        |> compute_accuracy ~labels:(Data.labels data)
      )
    | Data.Regression _ ->
      None
  in
  { Epoch_snapshot. loss; accuracy; }
;;

let train_model (type a)
    ~(model : a model)
    ~epochs:total_epochs
    ~(test_data: a Data.t)
    ~(validation_data: a Data.t) =
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
      let training =
        Some (gen_snapshot_entry ~session model model.training)
      in
      let validation =
        Some (gen_snapshot_entry ~session model validation_data)
      in
      let test =
        Some (gen_snapshot_entry ~session model test_data)
      in
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
    let feature_matrix =
      tensor_of_mat (Data.features model.training)
    in
    let target_matrix  =
      tensor_of_mat (Data.targets model.training)
    in

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
  let () =
    match test_data with
    | (Data.Classification _) as test_data ->
      let num_classes = Data.num_classes test_data in
      let arr = Array.create ~len:num_classes 0 in
      let labels = Data.labels test_data in
      Array.iter labels ~f:(fun lbl -> arr.(lbl) <- arr.(lbl) + 1);
      let baseline =
        Array.max_elt arr ~compare:Int.compare
        |> Option.value_exn
      in
      let baseline_accuracy =
        Float.(of_int baseline / of_int (Array.length labels))
      in
      Log.Global.info "Baseline test accuracy = %f" baseline_accuracy;
    | (Data.Regression _) as test_data ->
      let baseline_guesses =
        Owl.Mat.mean ~axis:0 (Data.targets model.training)
      in
      let baseline_test_loss =
        Owl.Mat.(
          mean' (sqr (Data.targets test_data - baseline_guesses))
        )
      in
      Log.Global.info "Baseline test loss = %f" baseline_test_loss;
  in
  Deferred.unit
;;

let target_matrix_of_labels ~num_classes labels = 
  let mat = Owl.Mat.create (Array.length labels) num_classes 0.0 in
  for i = 0 to Array.length labels - 1 do
    Owl.Mat.set mat i labels.(i) 1.0;
  done;
  mat
;;
