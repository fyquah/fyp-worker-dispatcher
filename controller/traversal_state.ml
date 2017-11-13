[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

open Common


type state =
  | Original
  | Override
[@@deriving sexp_of]

type pointer =
  | Root
  | Node_pointer of node_pointer
and node_pointer =
  { parent  : pointer;
    state   : state;
    node    : Inlining_tree.t;
  }

let rec sexp_of_pointer (pointer : pointer) =
  match pointer with
  | Root -> Sexp.Atom "Root"
  | Node_pointer node_pointer ->
    Sexp.List [
      Sexp.Atom "Node_pointer";
      sexp_of_node_pointer node_pointer;
    ]
and sexp_of_node_pointer node_pointer =
  let sexp_of_node (node : Inlining_tree.t) =
    match node with
    | Declaration decl ->
      Sexp.List [
        Sexp.Atom "Declaration";
        Closure_id.sexp_of_t decl.closure;
      ]

    | Apply_inlined_function inlined_function ->
      Sexp.List [
        Sexp.Atom "Apply_inlined_function";
        Closure_id.sexp_of_t inlined_function.applied;
      ]

    | Apply_non_inlined_function non_inlined_function ->
      Sexp.List [
        Sexp.Atom "Apply_non_inlined_function";
        Closure_id.sexp_of_t non_inlined_function.applied;
      ]
  in
  Sexp.List [
    Sexp.List [ Sexp.Atom "parent"; sexp_of_pointer node_pointer.parent ];
    Sexp.List [ Sexp.Atom "state";  sexp_of_state node_pointer.state ];
    Sexp.List [ Sexp.Atom "node";   sexp_of_node node_pointer.node ];
  ]

type t =
  { pointer   : pointer;
    tree_root : Inlining_tree.Top_level.t;
  }
[@@deriving sexp_of]

let rec compute_path pointer ~acc =
  match pointer with
  | Root -> acc
  | Node_pointer node_pointer ->
    compute_path node_pointer.parent ~acc:(node_pointer.node :: acc)

let change_call_site_source call_site ~source
    : Fyp_compiler_lib.Call_site.t =
  let source = Some source in
  match (call_site : Fyp_compiler_lib.Call_site.t) with
  | Enter_decl enter_decl -> Enter_decl { enter_decl with source }
  | At_call_site acs -> At_call_site { acs with source }

let node_closure_id = function
  | Inlining_tree.Declaration decl -> decl.closure
  | Apply_inlined_function a -> a.applied
  | Apply_non_inlined_function a -> a.applied

let to_override_rules t : Fyp_compiler_lib.Data_collector.t list =
  let pointer = t.pointer in
  let rec loop path =
    let match_hd (hd : Inlining_tree.t) ~remainder ~decision
        : (Call_site.t list * bool) =
      match hd with
      | Declaration decl ->
        let source = None in
        let closure = decl.closure in
        (Call_site.Enter_decl { source; closure; } :: remainder,
         decision)
      | Apply_inlined_function inlined_function ->
        let offset = inlined_function.offset in
        let applied = inlined_function.applied in
        let at_call_site =
          { Call_site.
            source = None;
            offset = offset;
            applied = applied;
          }
        in
        (Call_site.At_call_site at_call_site :: remainder, decision)
      | Apply_non_inlined_function non_inlined_function ->
        let offset = non_inlined_function.offset in
        let applied = non_inlined_function.applied in
        let at_call_site =
          { Fyp_compiler_lib.Call_site.
            source = None; offset; applied;
          }
        in
        (Call_site.At_call_site at_call_site :: remainder, decision)
    in
    match (path : Inlining_tree.t list) with
    | hd :: [] ->
      let decision =
        match hd with
        | Declaration _ -> assert false
        | Apply_inlined_function _ -> true
        | Apply_non_inlined_function _ -> false
      in
      match_hd hd ~remainder:[] ~decision
    | hd :: tl ->
      let call_stack, decision = loop tl in
      let source = node_closure_id hd in
      let call_stack =
        change_call_site_source ~source (List.hd_exn call_stack)
        :: List.tl_exn call_stack
      in
      match_hd hd ~remainder:call_stack ~decision
    | [] -> assert false
  in
  let path = compute_path pointer ~acc:[] in
  let call_stack, decision = loop path in
  let module Data_collector = Fyp_compiler_lib.Data_collector in
  let module Acc = struct
    type t =
      { rules       : Data_collector.t list;
        stack_so_far: Call_site.t list;
      }

    let empty = { rules = []; stack_so_far = [] }
  end
  in
  let rec loop_2 ~rev_stack ~(acc : Acc.t) =
    match rev_stack with
    | [] -> acc
    | hd :: tl ->
      let call_stack = acc.stack_so_far @ [ hd ] in
      let new_rule =
        match hd with
        | Call_site.Enter_decl _ -> None
        | Call_site.At_call_site acs ->
          Some (
            let applied = acs.applied in
            match tl with
            | [] -> { Data_collector. call_stack; applied; decision }
            | _  ->
              let decision = true in
              { Data_collector. call_stack; applied; decision }
          )
      in
      let rules =
        match new_rule with
        | None -> acc.Acc.rules
        | Some rule ->  rule :: acc.Acc.rules
      in
      let acc = { Acc. stack_so_far = call_stack; rules; } in
      loop_2 ~rev_stack:tl ~acc
  in
  (*
  Writer.write_sexp (Lazy.force Writer.stdout)
    (List.sexp_of_t (Inlining_tree.shallow_sexp_of_t) path);
  Writer.write (Lazy.force Writer.stdout) "\n";
  Writer.write_sexp (Lazy.force Writer.stdout)
    ([%sexp_of: Call_site.t list] call_stack);
  *)
  (loop_2 ~acc:Acc.empty ~rev_stack:(List.rev call_stack)).Acc.rules
;;

let refresh_tree_path
    (tree_root : Inlining_tree.Top_level.t)
    (path : Inlining_tree.t list) =
  let rec find_path_in_node
      (tree : Inlining_tree.t)
      (path : Inlining_tree.t list) =
    match path with
    | [] -> Some [ tree ]
    | hd :: tl ->
      match tree with
      | Apply_non_inlined_function _ -> None
      | Apply_inlined_function { children; _ }
      | Declaration { children; _ } ->
        let found =
          List.find_exn children ~f:(Inlining_tree.fuzzy_equal hd)
        in
        Option.map ~f:(fun a -> tree :: a) (find_path_in_node found tl)
  in
  match path with
  | [] -> failwith "Cannot match an empty path!"
  | otherwise ->
    List.find_map_exn tree_root ~f:(fun child ->
      find_path_in_node child otherwise)

let descent_pointer t state =
  let rec loop pointer =
    let match_first_child children =
      match children with
      | [] -> pointer
      | hd :: _ ->
        loop (Node_pointer { parent = pointer; state; node = hd; })
    in
    let match_node node =
      match (node : Inlining_tree.t) with
      | Declaration decl ->
        match_first_child decl.children
      | Apply_inlined_function inlined_function ->
        match_first_child inlined_function.children
      | Apply_non_inlined_function _ ->
        pointer
    in
    match pointer with
    | Root ->
      loop (
        let node = List.hd_exn t.tree_root in
        Node_pointer { parent = Root; state; node }
      )
    | Node_pointer node_pointer -> match_node node_pointer.node
  in
  loop t.pointer

let refresh_with_new_tree
    (t : t)
    (tree_root : Inlining_tree.Top_level.t) =
  let pointer = t.pointer in
  let path = compute_path pointer ~acc:[] in
  let refreshed_tree_path = refresh_tree_path tree_root path in
  let rec refresh_pointer pointer rpath =
    match pointer , rpath with
    | Root, [] -> Root
    | Node_pointer node_pointer, hd :: tl ->
      let new_parent = refresh_pointer pointer tl in
      Node_pointer { node_pointer with node = hd; parent = new_parent; }
    | Root, _ -> failwith "Shouldn't happen"
    | Node_pointer _, [] -> failwith "Shouldn't happen"
  in
  let pointer = refresh_pointer pointer (List.rev refreshed_tree_path) in
  let pointer = descent_pointer { pointer; tree_root; } Original in
  { pointer ; tree_root }
;;

let flip_node (node : Inlining_tree.t) : Inlining_tree.t =
  match node with
  | Declaration _ -> failwith "Cannot flip a declaration node"
  | Apply_inlined_function { applied; offset } ->
    Apply_non_inlined_function { Inlining_tree. applied; offset }
  | Apply_non_inlined_function { applied; offset; _ } ->
    Apply_inlined_function { applied; offset; children = [] }

let rec backtrack t =
  match t.pointer with
  | Root -> None
  | Node_pointer node_pointer ->
    let siblings =
      match node_pointer.parent with
      | Root -> t.tree_root
      | Node_pointer node_pointer ->
        match node_pointer.node with
        | Declaration d -> d.children
        | Apply_inlined_function inlined_function -> inlined_function.children
        | Apply_non_inlined_function _ ->
          failwith "Shouldn't have arrived here"
    in
    let index, (_ : Inlining_tree.t) =
      Option.value_exn (
        List.findi ~f:(fun (_ : int) a ->
            match t.pointer with
            | Node_pointer node_pointer ->
              Inlining_tree.fuzzy_equal a node_pointer.node
            | Root -> false)
          siblings
      )
    in
    if index >= List.length siblings - 1
    then begin
      match node_pointer.parent with
      | Root -> None
      | Node_pointer parent_node_pointer ->
        match parent_node_pointer.node, parent_node_pointer.state with
        | Declaration _, _
        | _, Override ->
          backtrack
            { t with pointer = Node_pointer parent_node_pointer }
        | node, Original ->
          let node = flip_node node in
          let pointer =
            Node_pointer { node_pointer with state = Override; node }
          in
          Some { t with pointer }
    end else
      let node = List.nth_exn siblings (index + 1) in
      let parent = node_pointer.parent in
      let pointer = Node_pointer { parent; state = Original; node } in
      let pointer = descent_pointer { t with pointer } Original in
      let pointer =
        match pointer with
        | Node_pointer node_pointer ->
          Node_pointer { node_pointer with state = Override }
        | Root -> assert false
      in
      Some { t with pointer }

let step (t : t) =
  let pointer = t.pointer in
  match pointer with
  | Root -> None
  | Node_pointer node_pointer ->
    match node_pointer.state with
    | Original ->
      (* The leaf node we are looking at has just been generated,
       * so we should move it to an override state to explore possible
       * inlining opportunities there.
       *)
      let node = flip_node node_pointer.node in
      let pointer =
        Node_pointer { node_pointer with state = Override; node }
      in
      Some { t with pointer }

    | Override ->
      (* Our previous run was overiding this node. It is possible that
       * we are still here because there is no further call sites
       * beneath. This means we should backtrack.
       *)
      backtrack t
;;

let next (t : t) (new_tree_root : Inlining_tree.Top_level.t) =
  step (refresh_with_new_tree t new_tree_root)

let init (tree_root : Inlining_tree.Top_level.t) =
  let t = { pointer = Root; tree_root; } in
  { t with pointer = descent_pointer t Original }
