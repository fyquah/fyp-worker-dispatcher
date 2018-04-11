[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type call_context =
  | Conditional_branch
  | String_switch_branch
  | Switch_int_branch
  | Switch_block_branch
  | Switch_failaction_branch
  | Imperative_loop
  | In_try_block
  | In_catch_block
  | Inlined_function
  | In_function_declaration

type wsb = {
    (* These are used by FLambda in WSB *)
    round                            : int;
    toplevel                         : bool;
    branch_depth                     : int;
    lifting                          : bool;
    original_size                    : int;
    new_size                         : int;
    benefit_remove_call              : int;
    benefit_remove_alloc             : int;
    benefit_remove_prim              : int;
    benefit_remove_branch            : int;
    benefit_direct_call_of_indirect  : int;
}

type trace_item =
  | Decl of Closure_origin.t
  | Apply of Apply_id.t


type t =
  { trace                            : trace_item list;
    flambda_wsb                      : wsb;

    (* callee features *)
    params                           : int;
    bound_vars_to_symbol             : int;
    assign                           : int;
    bound_vars_to_mutable            : int;
    bound_vars                       : int;
    free_vars                        : int;
    free_symbols                     : int;
    set_of_closures                  : int;
    is_a_functor                     : bool;
    non_specialized_args             : int;
    specialized_args                 : int;
    size_before_simplify             : int;
    size_after_simplify              : int;
    underlying_direct_applications   : int;
    underlying_indirect_applications : int;
    is_recursive                     : bool;
    expected_allocations             : float option;
    is_annonymous                    : bool;
    if_then_else                     : int;
    switch                           : int;
    string_switch                    : int;

    (* caller features *)
    call_context_stack               : call_context list;
    direct_call                      : bool;
    recursive_call                   : bool;
    only_use_of_function             : bool;

    (* environment features -- this is same for all siblings *)
    inlining_depth                   : int;
    closure_depth                    : int;
    in_recursive_function            : bool;
    original_function_size           : int option;
    original_bound_vars              : int option;
    flambda_round                    : int;
    flambda_tries                    : bool;
  }

let empty
    (* callee information *)
    ~params ~is_a_functor ~is_recursive ~is_annonymous
    ~size_before_simplify ~size_after_simplify

    (* caller information *)
    ~call_context_stack ~direct_call ~recursive_call

    (* env information *)
    ~inlining_depth ~closure_depth ~in_recursive_function
    ~original_function_size ~original_bound_vars ~flambda_round
    ~flambda_tries ~flambda_wsb ~trace ~only_use_of_function
  =
  { (* callee features *)
    params                           ;
    bound_vars_to_symbol             = 0;
    assign                           = 0;
    bound_vars_to_mutable            = 0;
    bound_vars                       = 0;
    free_vars                        = 0;
    free_symbols                     = 0;
    set_of_closures                  = 0;
    is_a_functor                     ;
    non_specialized_args             = 0;
    specialized_args                 = 0;
    size_before_simplify             ;
    size_after_simplify              ;
    underlying_direct_applications   = 0;
    underlying_indirect_applications = 0;
    is_recursive                     ;
    expected_allocations             = None;
    is_annonymous                    ;
    if_then_else                     = 0;
    switch                           = 0;
    string_switch                    = 0;

    (* caller features *)
    call_context_stack               ;
    direct_call                      ;
    recursive_call                   ;
    only_use_of_function;

    (* environment features -- this is same for all siblings *)
    inlining_depth                   ;
    closure_depth                    ;
    in_recursive_function            ;
    original_function_size           ;
    original_bound_vars              ;
    flambda_round                    ;
    flambda_tries                    ;

    flambda_wsb                      ;
    trace                            ;
  }

let (mined_features : t list ref) = ref []
