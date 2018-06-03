open Core

type feature_versions = [ `V0 | `V1 ]

module Manual_features_v1 = Fyp_compiler_plugins.Manual_features_v1
module Feature_utils = Fyp_compiler_plugins.Feature_utils

module Feature_list = struct
  include Fyp_compiler_plugins.Feature_utils.Feature_list

  let to_map a =
    String.Map.of_alist_exn (to_list a)
  ;;

  let sexp_of_t f t =
    List.sexp_of_t (fun (a, b) ->
        Sexp.List [ String.sexp_of_t a; f b; ])
      (to_list t)
  ;;
end

module Features = struct
  include Fyp_compiler_plugins.Feature_utils.Features

  let create_normaliser_to_owl_vec normaliser =
    let (`Staged f) = (create_normaliser_function normaliser) in
    Staged.stage (
      fun raw_features ->
        f raw_features
        |> to_array
        |> (fun a -> Owl.Mat.of_array a 1 (Array.length a))
    )
  ;;

  let sexp_of_t { int_features; numeric_features; bool_features; } =
    let open Sexp in
    List [
      List [ Atom "int_features";     (Feature_list.sexp_of_t Int.sexp_of_t   int_features)];
      List [ Atom "numeric_features"; (Feature_list.sexp_of_t Float.sexp_of_t numeric_features)];
      List [ Atom "bool_features";    (Feature_list.sexp_of_t Bool.sexp_of_t  bool_features)];
    ]
  ;;
end

let query_trace = Fyp_compiler_plugins.Feature_utils.query_trace

let parse_version v =
  match v with
  | "V0" -> Some `V0
  | "V1" -> Some `V1
  | _    -> None
;;
