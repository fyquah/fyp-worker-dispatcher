[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

open Common

exception Build_error

type t =
  | Declaration of declaration
  | Apply_inlined_function of inlined_function
  | Apply_non_inlined_function of non_inlined_function
and declaration =
  { closure   : Closure_id.t;
    children  : t list;
  }
and non_inlined_function =
  { applied   : Closure_id.t;
    offset    : Call_site_offset.t;
  }
and inlined_function =
  { applied   : Closure_id.t;
    offset    : Call_site_offset.t;
    children  : t list;
  }
[@@deriving sexp]

let shallow_sexp_of_t sexp =
  let open Sexp in
  match sexp with
  | Declaration decl ->
    List [ Atom "Declaration"; Closure_id.sexp_of_t decl.closure; ]
  | Apply_inlined_function inlined_function ->
    List [
      Atom "Apply_inlined_function";
      Closure_id.sexp_of_t inlined_function.applied;
    ]
  | Apply_non_inlined_function non_inlined_function ->
    List [
      Atom "Apply_non_inlined_function";
      Closure_id.sexp_of_t non_inlined_function.applied;
    ]

module Top_level = struct
  type nonrec t = t list [@@deriving sexp]
end

let fuzzy_equal a b =
  match a, b with
  | Declaration a, Declaration b ->
    Closure_id.equal a.closure b.closure
  | Apply_inlined_function a, Apply_inlined_function b ->
    Closure_id.equal a.applied a.applied &&
    Call_site.Offset.equal a.offset b.offset
  | Apply_non_inlined_function a, Apply_non_inlined_function b ->
    Closure_id.equal a.applied a.applied &&
    Call_site.Offset.equal a.offset b.offset
  | Apply_inlined_function a, Apply_non_inlined_function b ->
    Closure_id.equal a.applied a.applied &&
    Call_site.Offset.equal a.offset b.offset
  | _ , _ -> false

let equal_t_and_call_site (t : t) (call_site : Call_site.t) =
  match (t, call_site) with
  | (Declaration decl, Enter_decl enter_decl) ->
    Closure_id.equal decl.closure enter_decl.closure
  | (Apply_inlined_function inlined_function, At_call_site at_call_site) ->
    Closure_id.equal inlined_function.applied at_call_site.applied &&
    Call_site.Offset.equal inlined_function.offset at_call_site.offset

  | (Apply_non_inlined_function non_inlined_function, At_call_site at_call_site) ->
    Closure_id.equal non_inlined_function.applied at_call_site.applied &&
    Call_site.Offset.equal non_inlined_function.offset at_call_site.offset

  | _, _ -> false

(* Only on short lists! *)
let rec find_and_replace ~f ~default l =
  match l with
  | [] -> [ default () ]
  | hd :: tl ->
    match f hd with
    | Some a -> a :: tl
    | None -> hd :: (find_and_replace ~f ~default tl)
;;

let add (top_level_tree : Top_level.t) (collected : Data_collector.t) =

  let rec add__generic (tree : t) (call_stack : Call_site.t list) =
    match call_stack with
    | [] -> assert false
    | hd :: tl ->
      match hd with
      | At_call_site at_call_site ->
        add__call_site tree at_call_site tl
      | Enter_decl enter_decl ->
        add__declaration tree enter_decl tl

  and add__declaration
      (tree : t)
      (enter_decl : Call_site.enter_decl)
      (call_stack : Call_site.t list) =
    match call_stack with
    | [] -> failwith "Declaration cannot be a leaf node"
    | otherwise ->
      let finder haystack =
        let default () =
          let declaration =
            { children = []; closure = enter_decl.closure; }
          in
          add__generic (Declaration declaration) otherwise
        in
        let f (child : t) =
          if equal_t_and_call_site child
              (Call_site.Enter_decl enter_decl)
          then Some (add__generic child otherwise)
          else None
        in
        find_and_replace haystack ~default ~f
      in
      match tree with
      | Declaration decl ->
        let children = finder decl.children in
        Declaration { decl with children }

      | Apply_inlined_function inlined_function ->
        let children = finder inlined_function.children in
        Apply_inlined_function { inlined_function with children }

      | Apply_non_inlined_function _ ->
        failwith "Inconsistent assumption"

  and add__call_site
      (tree : t)
      (call_site : Call_site.at_call_site)
      (call_stack : Call_site.t list) =
    let finder haystack =
      match call_stack with
      | [] ->
        find_and_replace haystack
          ~default:(fun () ->
            if collected.decision then
              let record =
                { children = [];
                  offset   = call_site.offset;
                  applied  = collected.applied;
                }
              in
              assert (
                Closure_id.equal collected.applied call_site.applied
              );
              Apply_inlined_function record
            else
              let record =
                { offset = call_site.offset;
                  applied = collected.applied;
                }
              in
              assert (
                Closure_id.equal collected.applied call_site.applied
              );
              Apply_non_inlined_function record)
          ~f:(fun (child : t) ->
            if equal_t_and_call_site child
              (Call_site.At_call_site call_site)
            then Some child
            else None)
      | otherwise ->
        find_and_replace haystack
          ~default:(fun () ->
            let record =
              { children = [];
                offset   = call_site.offset;
                applied  = call_site.applied;
              }
            in
            add__generic (Apply_inlined_function record) otherwise)
          ~f:(fun (child : t) ->
            if equal_t_and_call_site child
              (Call_site.At_call_site call_site)
            then Some (add__generic child otherwise)
            else None)
    in
    match tree with
    | Declaration decl ->
      let children = finder decl.children in
      Declaration { decl with children }

    | Apply_inlined_function inlined_function ->
      let children = finder inlined_function.children in
      Apply_inlined_function { inlined_function with children }

    | Apply_non_inlined_function _ ->
      failwith "Inconsistent assumption"
  in
  let closure =
    let ident = Fyp_compiler_lib.Ident.create_persistent "<none>" in
    let linkage_name = Fyp_compiler_lib.Linkage_name.create "none" in
    let current_compilation_unit =
      Fyp_compiler_lib.Compilation_unit.create ident linkage_name
    in
    let variable =
      Fyp_compiler_lib.Variable.create ~current_compilation_unit
        "transient"
    in
    Closure_id.wrap variable
  in
  let children = top_level_tree in
  let call_site = Declaration { closure ; children } in
  match
    add__generic call_site (List.rev collected.call_stack)
  with
  | Declaration { closure = _; children } -> children
  | _ -> assert false
;;

let build (decisions : Data_collector.t list) : Top_level.t =
  let (init : Top_level.t) = [] in
  List.fold decisions ~init ~f:add
