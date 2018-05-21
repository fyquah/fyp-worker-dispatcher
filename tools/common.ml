open Core

type feature_versions = [ `V0 | `V1 ]

module Manual_features_v1 = Fyp_compiler_plugins.Manual_features_v1

module Feature_list = struct
  include Fyp_compiler_plugins.Feature_utils.Feature_list

  let to_map a =
    String.Map.of_alist_exn (to_list a)
  ;;
end

module Features = struct
  include Fyp_compiler_plugins.Feature_utils.Features

  let create_normaliser_to_owl_vec features_list =
    let (`Staged f) = (create_normaliser features_list) in
    Staged.stage (
      fun raw_features ->
        f raw_features
        |> to_array
        |> (fun a -> Owl.Mat.of_array a 1 (Array.length a))
    )
  ;;
end

let query_trace = Fyp_compiler_plugins.Feature_utils.query_trace

let parse_version v =
  match v with
  | "V0" -> Some `V0
  | "V1" -> Some `V1
  | _    -> None
;;
