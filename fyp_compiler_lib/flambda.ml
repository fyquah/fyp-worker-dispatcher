[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type call_kind =
  | Indirect
  | Direct of Closure_id.t

type const =
  | Int of int
  | Char of char
  | Const_pointer of int

type apply = {
  apply_id: Apply_id.t;
  func : Variable.t;
  args : Variable.t list;
  kind : call_kind;
  dbg : Debuginfo.t;
  inline : Lambda.inline_attribute;
  specialise : Lambda.specialise_attribute;
}

type assign = {
  being_assigned : Mutable_variable.t;
  new_value : Variable.t;
}

type send = {
  kind : Lambda.meth_kind;
  meth : Variable.t;
  obj : Variable.t;
  args : Variable.t list;
  dbg : Debuginfo.t;
}

type project_closure = Projection.project_closure
type move_within_set_of_closures = Projection.move_within_set_of_closures
type project_var = Projection.project_var

type specialised_to = {
  var : Variable.t;
  projection : Projection.t option;
}

type t =
  | Var of Variable.t
  | Let of let_expr
  | Let_mutable of let_mutable
  | Let_rec of (Variable.t * named) list * t
  | Apply of apply
  | Send of send
  | Assign of assign
  | If_then_else of Variable.t * t * t
  | Switch of Variable.t * switch
  | String_switch of Variable.t * (string * t) list * t option
  | Static_raise of Static_exception.t * Variable.t list
  | Static_catch of Static_exception.t * Variable.t list * t * t
  | Try_with of t * Variable.t * t
  | While of t * t
  | For of for_loop
  | Proved_unreachable

and named =
  | Symbol of Symbol.t
  | Const of const
  | Allocated_const of Allocated_const.t
  | Read_mutable of Mutable_variable.t
  | Read_symbol_field of Symbol.t * int
  | Set_of_closures of set_of_closures
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Project_var of project_var
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t
  | Expr of t

and let_expr = {
  var : Variable.t;
  defining_expr : named;
  body : t;
  free_vars_of_defining_expr : Variable.Set.t;
  free_vars_of_body : Variable.Set.t;
}

and let_mutable = {
  var : Mutable_variable.t;
  initial_value : Variable.t;
  contents_kind : Lambda.value_kind;
  body : t;
}

and set_of_closures = {
  function_decls : function_declarations;
  free_vars : specialised_to Variable.Map.t;
  specialised_args : specialised_to Variable.Map.t;
  direct_call_surrogates : Variable.t Variable.Map.t;
}

and function_declarations = {
  set_of_closures_id : Set_of_closures_id.t;
  set_of_closures_origin : Set_of_closures_origin.t;
  funs : function_declaration Variable.Map.t;
  specialised_for : Apply_id.t option;
}

and function_declaration = {
  closure_origin: Closure_origin.t;
  params : Parameter.t list;
  body : t;
  free_variables : Variable.Set.t;
  free_symbols : Symbol.Set.t;
  stub : bool;
  dbg : Debuginfo.t;
  inline : Lambda.inline_attribute;
  specialise : Lambda.specialise_attribute;
  is_a_functor : bool;
}

and switch = {
  numconsts : Numbers.Int.Set.t;
  consts : (int * t) list;
  numblocks : Numbers.Int.Set.t;
  blocks : (int * t) list;
  failaction : t option;
}

and for_loop = {
  bound_var : Variable.t;
  from_value : Variable.t;
  to_value : Variable.t;
  direction : Asttypes.direction_flag;
  body : t
}

and constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * constant_defining_value_block_field list
  | Set_of_closures of set_of_closures  (* [free_vars] must be empty *)
  | Project_closure of Symbol.t * Closure_id.t

and constant_defining_value_block_field =
  | Symbol of Symbol.t
  | Const of const

type expr = t

type program_body =
  | Let_symbol of Symbol.t * constant_defining_value * program_body
  | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program_body
  | Initialize_symbol of Symbol.t * Tag.t * t list * program_body
  | Effect of t * program_body
  | End of Symbol.t

type program = {
  imported_symbols : Symbol.Set.t;
  program_body : program_body;
}
