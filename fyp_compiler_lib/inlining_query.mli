module DC = Data_collector

type scope = Current | Outer

type result = {
    approx : Simple_value_approx.t;
    used_static_exceptions : Static_exception.Set.t;
    inlining_threshold : Inlining_cost.Threshold.t option;
    benefit : Inlining_cost.Benefit.t;
    num_direct_applications : int;
  }

type env = {
    round : int;
    approx : (scope * Simple_value_approx.t) Variable.Map.t;
    approx_mutable : Simple_value_approx.t Mutable_variable.Map.t;
    approx_sym : Simple_value_approx.t Symbol.Map.t;
    projections : Variable.t Projection.Map.t;
    current_closure : DC.Function_metadata.t option;
    current_functions : Set_of_closures_origin.Set.t;
    (* The functions currently being declared: used to avoid inlining
       recursively *)
    inlining_stack: (Call_site.t * DC.Trace_item.t) list;
    inlining_level : int;
    (* Number of times "inline" has been called recursively *)
    inside_branch : int;
    freshening : Freshening.t;
    never_inline : bool ;
    never_inline_inside_closures : bool;
    never_inline_outside_closures : bool;
    unroll_counts : int Set_of_closures_origin.Map.t;
    inlining_counts : int Real_closure_origin.Map.t;
    actively_unrolling : int Set_of_closures_origin.Map.t;
    closure_depth : int;
    inlining_stats_closure_stack : Inlining_stats.Closure_stack.t;
    inlined_debuginfo : Debuginfo.t;

    (* For feature extraction *)
    call_context_stack: Feature_extractor.call_context list;
    original_function_size_stack : int list;  (* Note : doesn't include size of inlined stuff *)
    original_bound_vars_stack : int list;
  }

type inlined_result =
  { r                        : result;
    body                     : Flambda.t;
  }

type query =
  { function_decl            : Flambda.function_declaration;
    closure_id_being_applied : Closure_id.t;
    env                      : env;
    r                        : result;
    apply_id                 : Apply_id.t;
    args                     : Variable.t list;
    original                 : Flambda.t;
    inlined_result           : inlined_result;
    call_kind                : Flambda.call_kind;
    value_set_of_closures    : Simple_value_approx.value_set_of_closures;
    only_use_of_function     : bool;
    wsb                      : Feature_extractor.wsb;
  }

val extract_v0_features : query -> Feature_extractor.t
val realise : query -> unit
