open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils
open Common

open Tensorflow_fnn

let num_classes = 2

let construct_tf_model ~(hyperparams : Tf_helper.hyperparams) num_features =
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
    |> O.softmax ~name:"network_output"
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
  { Tf_helper.
    is_training_placeholder;
    input_placeholder;
    target_placeholder;
    output;
    vars;
    loss;
    gd;
  }
;;

type 'a example = ('a Features.t * int)

let filter_eligible_examples (examples : [`raw] Raw_data.example list) =
  List.filter_map examples ~f:(fun (f, (a, b)) ->
    match Option.both a b with
    | None -> None
    | Some (a, no_inline) -> 
      if a.long_term >. no_inline then
        Some (f, 1)
      else
        Some (f, 0))
;;


let create_model ~hyperparams (examples: [`raw] example list) =
  let raw_features = Array.of_list_map examples ~f:fst in
  let labels  = Array.of_list_map examples ~f:snd in
  let normaliser = Features.create_normaliser (Array.to_list raw_features) in
  let create_normalised_feature_vector =
    Staged.unstage (
      Features.create_normaliser_to_owl_vec normaliser
    )
  in
  let feature_matrix =
    List.map examples ~f:fst
    |> Array.of_list_map ~f:create_normalised_feature_vector
    |> Owl.Mat.concatenate ~axis:0
  in
  let num_features = Owl.Mat.col_num feature_matrix in
  let target_matrix =
    Tf_helper.target_matrix_of_labels ~num_classes labels
  in
  let tf_model = construct_tf_model ~hyperparams num_features in
  let training =
    let features = feature_matrix in
    let targets  = target_matrix in
    Tf_helper.Data.Classification { features; labels; targets; }
  in
  { Tf_helper.
    normaliser; tf_model; create_normalised_feature_vector; training;
  }
;;

let do_analysis (examples : [`raw] example list)
    ~dump_normaliser ~checkpoint ~dump_graph ~hyperparams ~epochs ~(test_examples : [`raw] example list) =
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
    let labels = Array.of_list_map ~f:snd examples in
    let targets = Tf_helper.target_matrix_of_labels ~num_classes labels in
    Tf_helper.Data.Classification { features; labels; targets; }
  in
  let test_data = generate_features_and_labels test_examples in
  let validation_data =
    generate_features_and_labels validation_examples
  in
  let%bind () =
    Tf_helper.train_model ~dump_normaliser ~checkpoint ~dump_graph ~epochs ~validation_data ~test_data ~model
  in
  Deferred.return ()
;;

let print_labels_distribution name (examples : 'a example list) =
  let ret = Array.create ~len:num_classes 0 in
  List.iter examples ~f:(fun (_, i) ->
    ret.(i) <- ret.(i) + 1);
  let msg =
    Sexp.List (
      Array.mapi ret ~f:(fun i v ->
        Sexp.List [
          Sexp.Atom (Int.to_string i);
          Sexp.Atom (Float.(to_string (of_int v / of_int (List.length examples))));
        ])
      |> Array.to_list
    )
  in
  let msg =
    Sexp.List [
      Sexp.List [ Sexp.Atom "name"; Sexp.Atom name ];
      Sexp.List [ Sexp.Atom "distribution"; msg ];
    ]
  in
  Log.Global.sexp ~level:`Info msg
;;
  
let command =
  let open Command.Let_syntax in
  Command.async ~summary:"Linear reward model"
    [%map_open
      let {
        specification_file;
        epochs;
        hyperparams_file;
        feature_version;
        dump_graph;
        checkpoint;
        dump_normaliser;
      } = Command_params.training
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind specification =
          Reader.load_sexp_exn specification_file
            Specification_file.t_of_sexp
        in
        let%bind hyperparams =
          Reader.load_sexp_exn hyperparams_file [%of_sexp: Tf_helper.hyperparams]
        in
        let%bind training_examples, test_examples =
          load_from_specification ~version:feature_version specification
          >>| fun (a, b) ->
          (filter_eligible_examples a, filter_eligible_examples b)
        in
        let wait sec = Clock.after (Time.Span.of_sec sec) in
        Log.Global.sexp ~level:`Info [%message (hyperparams: Tf_helper.hyperparams)];
        let%bind () = wait 0.1 in
        print_labels_distribution "training examples" training_examples;
        print_labels_distribution "test examples" test_examples;

        (* Real analysis begins here. *)
        do_analysis ~dump_normaliser ~checkpoint ~dump_graph ~hyperparams ~epochs ~test_examples training_examples
    ]
;;
