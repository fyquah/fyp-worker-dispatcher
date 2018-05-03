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
