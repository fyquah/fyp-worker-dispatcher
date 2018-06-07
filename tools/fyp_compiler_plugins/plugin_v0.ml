module Features = Feature_utils.Features
module Feature_list = Feature_utils.Feature_list

open Model_loader

let unpack_binary_classification ~tau t =
  match t with
  | Tf_lib.Vec (a : float array) ->
    assert (Array.length a = 2);
    assert (abs_float (1.0 -. (a.(0) +. a.(1))) < 0.0001);
    a.(1) > tau
  | _ -> assert false
;;

let flip f = (fun a b -> f b a)

let to_vec features = Tf_lib.Vec (Features.to_array features)


let f_0 (query : Inlining_query.query) =
  let (v1_features : [`raw] Features.t) = Manual_features_v1.process query in
  let (v2_features : [`raw] Features.t) = Manual_features_v2.process query in
  (* assert (List.length (Feature_list.to_list features.int_features) = 0); *)
  let select = function
    | `V1 -> v1_features
    | `V2 -> v2_features
  in
  let familiar =
    let { Features. int_features; numeric_features; bool_features; } =
      select Familiarity_model.feature_version
    in
    let tau =
      match Sys.getenv_opt "FAMILIARITY_TAU" with
      | None -> 0.5
      | Some x -> float_of_string x
    in
    Familiarity_model.model ~int_features ~numeric_features ~bool_features
    |> unpack_binary_classification ~tau
  in
  if familiar then begin
    (* Some Data_collector.Action.Inline *)
    let { Features. int_features; numeric_features; bool_features; } =
      select Decision_model.feature_version
    in
    let decision =
      let tau =
        match Sys.getenv_opt "DECISION_TAU" with
        | None -> 0.5
        | Some x -> float_of_string x
      in
      Decision_model.model ~int_features ~numeric_features ~bool_features
      |> unpack_binary_classification ~tau
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
    if query.env.inlining_level > 20 then
      Some Data_collector.Action.Apply
    else
      f_0 query
  else
    None
;;


let () =
  Inlining_decision.init_custom_heuristic f
;;
