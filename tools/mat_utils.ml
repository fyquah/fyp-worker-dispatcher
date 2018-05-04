open Core
open Owl
open Tensorflow

module O = Ops

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
