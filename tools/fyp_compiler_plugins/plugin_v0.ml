module Features = Feature_utils.Features
module Feature_list = Feature_utils.Feature_list

let unpack_binary_classification t =
  match t with
  | Tf_lib.Vec (a : float array) ->
    assert (Array.length a = 2);
    a.(0) < a.(1)
  | _ -> assert false
;;

let (`Staged normalise_for_familiarity) =
  Features.create_normaliser_function Familiarity_normaliser.normaliser
;;

let (`Staged normalise_for_decision) =
  Features.create_normaliser_function Decision_normaliser.normaliser
;;

let flip f = (fun a b -> f b a)

let to_vec features = Tf_lib.Vec (Features.to_array features)

let f_0 (query : Inlining_query.query) =
  let features = Manual_features_v1.process query in
  (* assert (List.length (Feature_list.to_list features.int_features) = 0); *)
  let is_training = Tf_lib.Scalar 0 in
  let familiar =
    features
    |> normalise_for_familiarity
    |> to_vec
    |> flip Familiarity_model.feed_forward is_training
    |> unpack_binary_classification
  in
  if familiar then begin
    let decision =
      features
      |> normalise_for_decision
      |> to_vec
      |> flip Decision_model.feed_forward is_training
      |> unpack_binary_classification
    in 
    if decision then begin
      Some Data_collector.Action.Inline
    end else begin
      Some Data_collector.Action.Apply
    end
  end else
    None
;;

let f (query : Inlining_query.query) =
  if query.env.round = 0 then
    f_0 query
  else
    None
;;


let () =
  Inlining_decision.init_custom_heuristic f
;;
