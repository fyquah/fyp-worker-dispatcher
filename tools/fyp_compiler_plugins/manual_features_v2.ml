open Import

module Feature_list = Feature_utils.Feature_list
module Features = Feature_utils.Features

let process (query : Inlining_query.query) =
  let v1_features = Manual_features_v1.process query in
  let wsb_features =
    let wsb = query.wsb in
    let bool_features =
      (* these features are probably unnecessary *)
      Feature_list.of_list [
        ("toplevel", wsb.toplevel);
        ("lifting", wsb.lifting);
      ]
    in
    let numeric_features =
      [("benefit_remove_call", wsb.benefit_remove_call);
       ("benefit_remove_alloc", wsb.benefit_remove_alloc);
       ("benefit_remove_prim", wsb.benefit_remove_prim);
       ("benefit_remove_branch", wsb.benefit_remove_branch);
       ("benefit_direct_call_of_indirect", wsb.benefit_direct_call_of_indirect)]
      |> List.map (fun (a, b) -> (a, float_of_int b))
      |> Feature_list.of_list
    in
    let int_features = Feature_list.empty in
    { Features. bool_features; numeric_features; int_features; }
  in
  Features.(v1_features @ wsb_features)
;;
