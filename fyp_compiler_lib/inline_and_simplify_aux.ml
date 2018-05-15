module DC = Data_collector

module Env = struct
  type scope = Current | Outer

  type t = {
    backend : (module Backend_intf.S);
    call_site_offset : Call_site.Offset.t ref;
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
    inlining_counts : int Closure_origin.Map.t;
    actively_unrolling : int Set_of_closures_origin.Map.t;
    closure_depth : int;
    inlining_stats_closure_stack : Inlining_stats.Closure_stack.t;
    inlined_debuginfo : Debuginfo.t;

    (* For feature extraction *)
    call_context_stack: Feature_extractor.call_context list;
    original_function_size_stack : int list;  (* Note : doesn't include size of inlined stuff *)
    original_bound_vars_stack : int list;

    (* for custom inlining decisions *)
    overrides: Data_collector.Multiversion_overrides.t;
  }
end

module Result = struct
  type t =
    { approx : Simple_value_approx.t;
      used_static_exceptions : Static_exception.Set.t;
      inlining_threshold : Inlining_cost.Threshold.t option;
      benefit : Inlining_cost.Benefit.t;
      num_direct_applications : int;
    }
end
