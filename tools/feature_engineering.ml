open Core
open Async
open Protocol.Shadow_fyp_compiler_lib

module Absolute_path = Protocol.Absolute_path

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

let sqr x = x *. x

let is_small x = Float.abs x <. 0.000001

let create_numerical_features (feature_vector : Feature_extractor.t) =
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
  let simple_discrt = discretise_int_feature in
  let boolean_features =
    Array.concat [
      simple_boolean_features;
      simple_discrt ~lo:0 ~hi:8 feature_vector.flambda_wsb.branch_depth;
      simple_discrt ~lo:0 ~hi:8 feature_vector.inlining_depth;
      simple_discrt ~lo:0 ~hi:8 feature_vector.closure_depth;
    ]
  in
  (int_features, boolean_features)
;;

let create_feature_transformer (features: Feature_extractor.t list) =
  let processed_features =
    Array.of_list_map ~f:create_numerical_features features
  in
  let num_int_features = fst (processed_features.(0)) |> Array.length in
  let num_bool_features = snd (processed_features.(0)) |> Array.length in
  let num_features = num_int_features + num_bool_features in
  let module Mat = Owl.Mat in
  let feature_means =
    Array.map ~f:fst processed_features
    |> Array.map ~f:(Array.map ~f:Float.of_int)
    |> Owl.Mat.of_arrays
    |> (fun m ->
        if Mat.col_num m > 0 then
          Owl.Mat.mean_rows m
        else
          m)
    |> Owl.Mat.to_array
  in
  let feature_std =
    Array.map ~f:fst processed_features
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
      let (int_features, boolean_features) =
        create_numerical_features feature_set
      in
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
  Staged.stage create_normalised_feature_vector
;;
