open Import

module Feature_list = Feature_utils.Feature_list
module Features = Feature_utils.Features

let process (query : Inlining_query.query) =
  let v2_features = Manual_features_v1.process query in
  let structural_features ~prefix ~toplevel body =
    let num_apply                       = ref 0 in
    let num_send                        = ref 0 in
    let num_if_then_else                = ref 0 in
    let num_switch                      = ref 0 in
    let num_string_switch               = ref 0 in
    let num_static_raise                = ref 0 in
    let num_try_with                    = ref 0 in
    let num_while                       = ref 0 in
    let num_for                         = ref 0 in
    let num_proved_unreachable          = ref 0 in
    let num_symbol                      = ref 0 in
    let num_const                       = ref 0 in
    let num_allocated_const             = ref 0 in
    let num_read_mutable                = ref 0 in
    let num_read_symbol_field           = ref 0 in
    let num_set_of_closures             = ref 0 in
    let num_project_closure             = ref 0 in
    let num_move_within_set_of_closures = ref 0 in
    let num_project_var                 = ref 0 in
    let num_prim                        = ref 0 in
    let incr r =  r := !r + 1 in
    Flambda.iter_general ~toplevel:toplevel
      (fun (t : Flambda.t) ->
        match t with
        | Apply _            -> incr num_apply
        | Send  _            -> incr num_send
        | If_then_else _     -> incr num_if_then_else
        | Switch _           -> incr num_switch 
        | String_switch _    -> incr num_string_switch
        | Static_raise _     -> incr num_static_raise
        | Try_with _         -> incr num_try_with
        | While _            -> incr num_while
        | For _              -> incr num_for
        | Proved_unreachable -> incr num_proved_unreachable
        | _ -> ())
      (fun (named : Flambda.named) ->
        match named with
        | Expr _                        -> ()
        | Symbol _                      -> incr num_symbol
        | Const _                       -> incr num_const
        | Allocated_const _             -> incr num_allocated_const
        | Read_mutable _                -> incr num_read_mutable
        | Read_symbol_field _           -> incr num_read_symbol_field
        | Set_of_closures _             -> incr num_set_of_closures
        | Project_closure _             -> incr num_project_closure
        | Move_within_set_of_closures _ -> incr num_move_within_set_of_closures
        | Project_var _                 -> incr num_project_var
        | Prim _                        -> incr num_prim)
      (Flambda.Is_expr body);
    let output =
      [("num_apply"                       , !num_apply                      );
       ("num_send"                        , !num_send                       );
       ("num_if_then_else"                , !num_if_then_else               );
       ("num_switch"                      , !num_switch                     );
       ("num_string_switch"               , !num_string_switch              );
       ("num_static_raise"                , !num_static_raise               );
       ("num_try_with"                    , !num_try_with                   );
       ("num_while"                       , !num_while                      );
       ("num_for"                         , !num_for                        );
       ("num_proved_unreachable"          , !num_proved_unreachable         );
       ("num_symbol"                      , !num_symbol                     );
       ("num_const"                       , !num_const                      );
       ("num_allocated_const"             , !num_allocated_const            );
       ("num_read_mutable"                , !num_read_mutable               );
       ("num_read_symbol_field"           , !num_read_symbol_field          );
       ("num_set_of_closures"             , !num_set_of_closures            );
       ("num_project_closure"             , !num_project_closure            );
       ("num_move_within_set_of_closures" , !num_move_within_set_of_closures);
       ("num_project_var"                 , !num_project_var                );
       ("num_prim"                        , !num_prim                       );
      ]
    in
    let numeric_features =
      List.map (fun (a, b) ->
          let toplevel =
            if toplevel then
              "toplevel"
            else
              "nested"
          in
          let a = prefix ^ "_" ^ toplevel ^ "_" ^ a in
          (a, float_of_int b))
        output
      |> Feature_list.of_list
    in
    let int_features = Feature_list.empty in
    let bool_features = Feature_list.empty in
    { Features. int_features; bool_features; numeric_features; }
  in
  Features.(
    v2_features
    @ structural_features ~toplevel:true  ~prefix:"original" query.original
    @ structural_features ~toplevel:true  ~prefix:"inlined"  query.inlined_result.body
  )
;;
