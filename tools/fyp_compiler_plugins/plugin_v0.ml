module Features = Feature_utils.Features
module Feature_list = Feature_utils.Feature_list

let unpack_binary_classification t =
  match t with
  | Tf_lib.Vec (a : float array) ->
    assert (Array.length a = 2);
    a.(0) < a.(1)
  | _ -> assert false
;;

let f (query : Inlining_query.query) =
  let features = Manual_features_v1.process query in
  assert (List.length features.int_features = 0);
  (* TODO: Normalisation *)
  let feature_vector = Tf_lib.Vec (Features.to_array features) in
  let is_training = Tf_lib.Scalar false in
  let familiar =
    Familiarity_model.feed_forward feature_vector is_training
    |> unpack_binary_classification
  in
  if familiar then begin
    let decision =
      Decision_model.feed_forward feature_vector is_training
      |> unpack_binary_classification
    in 
    if decision then
      Some Data_collector.Action.Inline
    else
      Some Data_collector.Action.Apply
  end else
    None
;;

let () =
  Inlining_decision.init_custom_heuristic f
;;
