open Core
open Owl
open Tensorflow
open Protocol.Shadow_fyp_compiler_lib
open Async

module O = Ops


let shape_to_string (a, b) = sprintf "(%d, %d)" a b

let tensor_of_mat mat =
  Tensorflow_core.Tensor.of_float_array2
    (Owl.Mat.to_arrays mat) Bigarray.Float32
;;

let tensor_to_mat data =
  Tensorflow_core.Tensor.to_float_array2 data
  |> Owl.Mat.of_arrays
;;

let tensor_of_int_array int_arr =
  let float_arr = Array.map ~f:Float.of_int int_arr in
  Tensorflow_core.Tensor.of_float_array1 (float_arr) Bigarray.Float32
;;

let tensor_of_bool b =
  let b = if b then 1 else 0 in
  Tensorflow_core.Tensor.create Bigarray.Int32  [| b |]
;;

let tensor_scalar_int32 a =
  let tensor = Tensorflow_core.Tensor.create Bigarray.Int32  [||] in
  Tensorflow_core.Tensor.set tensor [||] (Int32.of_int_exn 0);
  tensor
;;

let const_bool
    ?(name = "Const")
    ?(control_inputs = [])
    ?shape
    values
  =
  let get_shape ?shape values =
    match shape with
    | Some shape ->
      let vs = List.fold shape ~init:1 ~f:( * ) in
      let len = List.length values in
      if vs <> len
      then raise (Invalid_argument (Printf.sprintf "Input length mismatch %d <> %d" vs len));
      shape
    | None -> [ List.length values ]
  in
  let shape = get_shape ?shape values in
  let values = List.map ~f:(fun x -> if x then 1 else 0) values in
  Node.create
    ~name:(Node.Name.of_string name)
    ~op_name:(Node.Op_name.of_string "Const")
    ~output_type:Bool
    ~inputs:[]
    ~control_inputs
    ~attributes:[
      "dtype", Type (P Bool);
      "value", Tensor_int { type_ = P Bool; shape; values };
    ]
    ~output_idx:None


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

  type segmented = 
    { training : entry list;
      test     : entry list;
    }
  [@@deriving sexp]

  type t =
    | Segmented of segmented     (* To provide true out-of-sample testing *)
    | Unsegmented of entry list
  [@@deriving sexp]
end


type target = (Raw_reward.dual_reward option * float option)
type example = (Feature_extractor.t * target)

let load_call_site_examples
    (specification_entries: Specification_file.entry list) =
  Deferred.List.concat_map specification_entries ~f:(fun specification_entry ->
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

let load_from_specification specification =
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
;;
