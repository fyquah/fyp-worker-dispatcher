(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let all_free_symbols (function_decls : Flambda.function_declarations) =
  Variable.Map.fold (fun _ (function_decl : Flambda.function_declaration)
          syms ->
      Symbol.Set.union syms function_decl.free_symbols)
    function_decls.funs Symbol.Set.empty

let toplevel_substitution sb tree =
  let sb' = sb in
  let sb v = try Variable.Map.find v sb with Not_found -> v in
  let aux (flam : Flambda.t) : Flambda.t =
    match flam with
    | Var var ->
      let var = sb var in
      Var var
    | Let_mutable mutable_let ->
      let initial_value = sb mutable_let.initial_value in
      Let_mutable { mutable_let with initial_value }
    | Assign { being_assigned; new_value; } ->
      let new_value = sb new_value in
      Assign { being_assigned; new_value; }
    | Apply { apply_id; func; args; kind; dbg; inline; specialise; } ->
      let func = sb func in
      let args = List.map sb args in
      Apply { apply_id; func; args; kind; dbg; inline; specialise; }
    | If_then_else (cond, e1, e2) ->
      let cond = sb cond in
      If_then_else (cond, e1, e2)
    | Switch (cond, sw) ->
      let cond = sb cond in
      Switch (cond, sw)
    | String_switch (cond, branches, def) ->
      let cond = sb cond in
      String_switch (cond, branches, def)
    | Send { kind; meth; obj; args; dbg } ->
      let meth = sb meth in
      let obj = sb obj in
      let args = List.map sb args in
      Send { kind; meth; obj; args; dbg }
    | For { bound_var; from_value; to_value; direction; body } ->
      let from_value = sb from_value in
      let to_value = sb to_value in
      For { bound_var; from_value; to_value; direction; body }
    | Static_raise (static_exn, args) ->
      let args = List.map sb args in
      Static_raise (static_exn, args)
    | Static_catch _ | Try_with _ | While _
    | Let _ | Let_rec _ | Proved_unreachable -> flam
  in
  let aux_named (named : Flambda.named) : Flambda.named =
    match named with
    | Symbol _ | Const _ | Expr _ -> named
    | Allocated_const _ | Read_mutable _ -> named
    | Read_symbol_field _ -> named
    | Set_of_closures set_of_closures ->
      let set_of_closures =
        Flambda.create_set_of_closures
          ~function_decls:set_of_closures.function_decls
          ~free_vars:
            (Variable.Map.map (fun (spec_to : Flambda.specialised_to) ->
                { spec_to with var = sb spec_to.var; })
              set_of_closures.free_vars)
          ~specialised_args:
            (Variable.Map.map (fun (spec_to : Flambda.specialised_to) ->
                { spec_to with var = sb spec_to.var; })
              set_of_closures.specialised_args)
          ~direct_call_surrogates:set_of_closures.direct_call_surrogates
      in
      Set_of_closures set_of_closures
    | Project_closure project_closure ->
      Project_closure {
        project_closure with
        set_of_closures = sb project_closure.set_of_closures;
      }
    | Move_within_set_of_closures move_within_set_of_closures ->
      Move_within_set_of_closures {
        move_within_set_of_closures with
        closure = sb move_within_set_of_closures.closure;
      }
    | Project_var project_var ->
      Project_var {
        project_var with
        closure = sb project_var.closure;
      }
    | Prim (prim, args, dbg) ->
      Prim (prim, List.map sb args, dbg)
  in
  if Variable.Map.is_empty sb' then tree
  else Flambda_iterators.map_toplevel aux aux_named tree

