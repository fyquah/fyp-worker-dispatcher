open Core
open Protocol.Shadow_fyp_compiler_lib

val convert_v0_features : Feature_extractor.t -> [`raw] Common.Features.t

val discretise_int_feature : lo: int -> hi: int -> (string * int) -> (string * bool) array

(** Transformer that (1) transforms the feature extractor object into
 *  mathematical vectors and (2) does feature normalisation on non-boolean
 *  features.
 *)
val create_feature_transformer
  : Feature_extractor.t list -> (Feature_extractor.t -> Owl.Mat.mat) Staged.t
