(*
  placeholder_6 - (-1, 34)
  placeholder_8 - ()
*)
let model placeholder_6 placeholder_8 =
  let const_16 = Tf_lib.of_float -0.030316952616 in
  let variable_15 = Tf_lib.get_variable "Variable-15" [| 34; 32 |] in
  let add_17 = Tf_lib.add const_16 variable_15 in
  let matmul_22 = Tf_lib.matmul placeholder_6 add_17 in
  let variable_21 = Tf_lib.get_variable "Variable-21" [| 32 |] in
  let add_23 = Tf_lib.add matmul_22 variable_21 in
  let relu_24 = Tf_lib.relu add_23 in
  let const_26 = Tf_lib.of_float 0.5 in
  let const_9 = Tf_lib.of_int 0 in
  let notequal_10 = Tf_lib.notequal const_9 placeholder_8 in
  let switch_27 = Tf_lib.switch notequal_10 notequal_10 in
  let identity_29 = Tf_lib.identity switch_27 in
  let ^identity_29 = Tf_lib.bool_not identity_29 in
  let identity_30 = Tf_lib.identity const_26 ^identity_29 in
  let const_25 = Tf_lib.of_float 1.0 in
  let switch_28 = Tf_lib.switch notequal_10 notequal_10 in
  let identity_31 = Tf_lib.identity switch_28 in
  let ^identity_31 = Tf_lib.bool_not identity_31 in
  let identity_32 = Tf_lib.identity const_25 ^identity_31 in
  let merge_34 = Tf_lib.merge identity_30 identity_32 in
  let reciprocal_39 = Tf_lib.reciprocal merge_34 in
  let mul_40 = Tf_lib.mul relu_24 reciprocal_39 in
  let shape_35 = Tf_lib.shape relu_24 in
  let randomuniform_36 = Tf_lib.randomuniform shape_35 in
  let add_37 = Tf_lib.add merge_34 randomuniform_36 in
  let floor_38 = Tf_lib.floor add_37 in
  let mul_41 = Tf_lib.mul mul_40 floor_38 in
  let const_47 = Tf_lib.of_float -0.0441941730678 in
  let variable_46 = Tf_lib.get_variable "Variable-46" [| 32; 16 |] in
  let add_48 = Tf_lib.add const_47 variable_46 in
  let matmul_53 = Tf_lib.matmul mul_41 add_48 in
  let variable_52 = Tf_lib.get_variable "Variable-52" [| 16 |] in
  let add_54 = Tf_lib.add matmul_53 variable_52 in
  let relu_55 = Tf_lib.relu add_54 in
  let const_57 = Tf_lib.of_float 0.5 in
  let switch_58 = Tf_lib.switch notequal_10 notequal_10 in
  let identity_60 = Tf_lib.identity switch_58 in
  let ^identity_60 = Tf_lib.bool_not identity_60 in
  let identity_61 = Tf_lib.identity const_57 ^identity_60 in
  let const_56 = Tf_lib.of_float 1.0 in
  let switch_59 = Tf_lib.switch notequal_10 notequal_10 in
  let identity_62 = Tf_lib.identity switch_59 in
  let ^identity_62 = Tf_lib.bool_not identity_62 in
  let identity_63 = Tf_lib.identity const_56 ^identity_62 in
  let merge_65 = Tf_lib.merge identity_61 identity_63 in
  let reciprocal_70 = Tf_lib.reciprocal merge_65 in
  let mul_71 = Tf_lib.mul relu_55 reciprocal_70 in
  let shape_66 = Tf_lib.shape relu_55 in
  let randomuniform_67 = Tf_lib.randomuniform shape_66 in
  let add_68 = Tf_lib.add merge_65 randomuniform_67 in
  let floor_69 = Tf_lib.floor add_68 in
  let mul_72 = Tf_lib.mul mul_71 floor_69 in
  let const_78 = Tf_lib.of_float -0.176776692271 in
  let variable_77 = Tf_lib.get_variable "Variable-77" [| 16; 2 |] in
  let add_79 = Tf_lib.add const_78 variable_77 in
  let matmul_84 = Tf_lib.matmul mul_72 add_79 in
  let variable_83 = Tf_lib.get_variable "Variable-83" [| 2 |] in
  let add_85 = Tf_lib.add matmul_84 variable_83 in
  let network_output_86 = Tf_lib.softmax add_85 in
  network_output_86
;;
