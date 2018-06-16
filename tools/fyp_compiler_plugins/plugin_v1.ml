module Features = Feature_utils.Features
module Feature_list = Feature_utils.Feature_list

open Model_loader

let flip f = (fun a b -> f b a)

let to_vec features = Tf_lib.Vec (Features.to_array features)

(* - doesnt_matter : 0
 * - inline        : 1
 * - apply         : 2
 * *)

let f_0 (query : Inlining_query.query) =
  let (v1_features : [`raw] Features.t) = Manual_features_v1.process query in
  let (v2_features : [`raw] Features.t) = Manual_features_v2.process query in
  let (v3_features : [`raw] Features.t) = Manual_features_v3.process query in
  (* assert (List.length (Feature_list.to_list features.int_features) = 0); *)
  let select = function
    | `V1 -> v1_features
    | `V2 -> v2_features
    | `V3 -> v3_features
  in
  let label =
    let { Features. int_features; numeric_features; bool_features; } =
      select Decision_model.feature_version
    in
    Decision_model.model ~int_features ~numeric_features ~bool_features
    |> function
    | Vec v ->
      Tf_lib.argmax v
    | _ -> assert false
  in
  match label with
  | 0 -> None
  | 1 -> Some Data_collector.Action.Inline
  | 2 -> Some Data_collector.Action.Apply
;;


let f (query : Inlining_query.query) =
  if query.env.round = 0 then
    (* safety net: allow inline / unroll up to 30 iteres *)
    let inlining_count =
      try
        Closure_origin.Map.find query.function_decl.closure_origin query.env.inlining_counts
      with Not_found ->
        30
    in
    if query.env.inlining_level > 30 || inlining_count <= 0 then
      Some Data_collector.Action.Apply
    else
      f_0 query
  else
    None
;;

let () =
  assert (!Clflags.default_simplify_rounds = 3);  (* Don't use this plugin when not compiling in O3 *)
  Inlining_decision.init_custom_heuristic f
;;
