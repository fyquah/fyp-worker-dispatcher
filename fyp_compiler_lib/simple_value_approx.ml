type 'a boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type value_string = {
  (* CR-soon mshinwell: use variant *)
  contents : string option; (* None if unknown or mutable *)
  size : int;
}

type unresolved_value =
  | Set_of_closures_id of Set_of_closures_id.t
  | Symbol of Symbol.t

type unknown_because_of =
  | Unresolved_value of unresolved_value
  | Other

type t = {
  descr : descr;
  var : Variable.t option;
  symbol : (Symbol.t * int option) option;
}

and descr =
  | Value_block of Tag.t * t array
  | Value_int of int
  | Value_char of char
  | Value_constptr of int
  | Value_float of float option
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_closure
  | Value_string of value_string
  | Value_float_array of value_float_array
  | Value_unknown of unknown_because_of
  | Value_bottom
  | Value_extern of Export_id.t
  | Value_symbol of Symbol.t
  | Value_unresolved of unresolved_value
    (* No description was found for this value *)

and value_closure = {
  set_of_closures : t;
  closure_id : Closure_id.t;
}

and value_set_of_closures = {
  function_decls : Flambda.function_declarations;
  bound_vars : t Var_within_closure.Map.t;
  invariant_params : Variable.Set.t Variable.Map.t;
  size : int option Variable.Map.t;
  specialised_args : Flambda.specialised_to Variable.Map.t;
  freshening : Freshening.Project_var.t;
  direct_call_surrogates : Closure_id.t Closure_id.Map.t;
}

and value_float_array_contents =
  | Contents of t array
  | Unknown_or_mutable

and value_float_array = {
  contents : value_float_array_contents;
  size : int;
}
