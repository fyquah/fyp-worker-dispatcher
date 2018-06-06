type 'a t =
  | Mat of 'a array array
  | Vec of 'a array
  | Scalar of 'a
  | Nothing

val of_float : float -> float t
val of_int   : int   -> int t

(* boolean ops *)
val bool_not : bool t -> bool t
val eval_bool : bool t -> bool

val shape  : 'a t -> int array
val randomuniform : int array -> float t

(* neural net ops *)
val notequal  : 'a t -> 'a t -> bool t
val matmul     : float t -> float t -> float t
val add        : float t -> float t -> float t
val relu       : float t -> float t
val logistic   : float t -> float t
val softmax    : float t -> float t
val reciprocal : float t -> float t
val softmax    : float t -> float t
val floor    : float t -> float t
val mul        : float t -> float t -> float t

(* loading from other things *)
val get_variable : string -> int array -> float t
val merge        : 'a t -> 'a t -> 'a t
val unpack_scalar_exn : 'a t -> 'a

val features_to_t 
   : int_features: int Feature_utils.Feature_list.t
  -> numeric_features: float Feature_utils.Feature_list.t
  -> bool_features: bool Feature_utils.Feature_list.t
  -> numeric_features_indices: int array
  -> bool_features_indices: int array
  -> numeric_features_means: float array
  -> numeric_features_std: float array
  -> float t

val sigmoid : float -> float

val check_names : names : string array -> 'a Feature_utils.Feature_list.t -> unit
