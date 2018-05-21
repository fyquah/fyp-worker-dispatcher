type 'a t =
  | Mat of 'a array array
  | Vec of 'a array
  | Scalar of 'a

val of_float : float -> float t
val of_int   : int   -> int t

(* boolean ops *)
val bool_not : bool t -> bool t

val shape  : 'a t -> int array
val randomuniform : int array -> float t

(* neural net ops *)
val matmul     : float t -> float t -> float t
val add        : float t -> float t -> float t
val relu       : float t -> float t
val softmax    : float t -> float t
val reciprocal : float t -> float t
val softmax    : float t -> float t
