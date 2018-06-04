module Features = Feature_utils.Features
module Feature_list = Feature_utils.Feature_list

open Model_loader

let unpack_binary_classification t =
  match t with
  | Tf_lib.Vec (a : float array) ->
    assert (Array.length a = 2);
    a.(0) < a.(1)
  | _ -> assert false
;;

let flip f = (fun a b -> f b a)

let to_vec features = Tf_lib.Vec (Features.to_array features)

let f_0 (query : Inlining_query.query) =
  let (features : [`raw] Features.t) = Manual_features_v1.process query in
  (* assert (List.length (Feature_list.to_list features.int_features) = 0); *)
  let { Features. int_features; numeric_features; bool_features; } = features in
  let familiar =
    Familiarity_model.model ~int_features ~numeric_features ~bool_features
    |> unpack_binary_classification
  in
  if familiar then begin
    let decision =
      Decision_model.model ~int_features ~numeric_features ~bool_features
      |> unpack_binary_classification
    in 
    if decision then begin
      Format.eprintf "inline\n";
      Some Data_collector.Action.Inline
    end else begin
      Format.eprintf "Don't inline\n";
      Some Data_collector.Action.Apply
    end
  end else begin
    Format.eprintf "I don't know\n";
    None
  end
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
