[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Shadow_fyp_compiler_lib

exception Build_error

module V0 = struct

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
  [@@deriving sexp, compare]
  
  let is_leaf t =
    match t with
    | Apply_inlined_function { children = []; _ }
    | Declaration { children = []; _ }
    | Apply_non_inlined_function _ -> true
    | _ -> false
  ;;
  
  let flip_node t =
    match t with
    | Declaration _ -> assert false
    | Apply_inlined_function { applied; offset } ->
      Apply_non_inlined_function { applied; offset }
    | Apply_non_inlined_function { applied; offset } ->
      Apply_inlined_function { applied; offset; children = [] }
  ;;
  
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
    type root = t list [@@deriving sexp, compare]
  
    let count_leaves root =
      let rec loop t =
        let sum_all l =
          match l with
          | [] -> 1
          | l ->
            List.map ~f:loop l
            |> List.sum ~f:Fn.id (module Int)
        in
        match t with
        | Declaration decl -> sum_all decl.children
        | Apply_inlined_function inlined_function ->
          sum_all inlined_function.children
        | Apply_non_inlined_function _ -> 1
      in
      List.sum ~f:(fun t -> loop t) (module Int) root
    ;;
  
    let flip_nth_leaf root n =
      let rec loop ~state ~node =
        if is_leaf node then begin
          match state with
          | 0 -> `Replaced (flip_node node)
          | state -> `Leaf_visited (state - 1)
        end else begin
          match node with
          | Apply_non_inlined_function _ -> assert false
          | Declaration decl ->
            begin match loop_nodes ~state ~nodes:decl.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Replaced children ->
              `Replaced (Declaration { decl with children })
            end
          | Apply_inlined_function inlined ->
            begin match loop_nodes ~state ~nodes:inlined.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Replaced children ->
              `Replaced (
                Apply_inlined_function { inlined with children }
              )
            end
        end
      and loop_nodes ~state ~nodes =
        match nodes with
        | [] -> `Leaf_visited state
        | hd :: tl ->
          match loop ~state ~node:hd with
          | `Replaced node -> `Replaced (node :: tl)
          | `Leaf_visited i ->
            match loop_nodes ~state:i ~nodes:tl with
            | `Replaced tl -> `Replaced (hd :: tl)
            | `Leaf_visited i -> `Leaf_visited i
      in
      match loop_nodes ~state:n ~nodes:root with
      | `Leaf_visited i ->
        failwithf "Cannot visit leaf %d from in a tree with %d leaves"
          i n ()
      | `Replaced new_tree -> new_tree
    ;;
  
    let backtrack_nth_leaf root n =
      let rec loop ~state ~node =
        if is_leaf node then begin
          match state with
          | 0 -> `Candidate
          | state -> `Leaf_visited (state - 1)
        end else begin
          match node with
          | Apply_non_inlined_function _ -> assert false
          | Declaration decl ->
            begin match loop_nodes ~state ~nodes:decl.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Candidate -> `Candidate
            | `Replaced children ->
              `Replaced (Declaration { decl with children })
            end
          | Apply_inlined_function inlined ->
            begin match loop_nodes ~state ~nodes:inlined.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Candidate ->
              let applied = inlined.applied in
              let offset = inlined.offset in
              `Replaced (Apply_non_inlined_function { applied; offset; })
            | `Replaced children ->
              `Replaced (Apply_inlined_function { inlined with children })
            end
        end
      and loop_nodes ~state ~nodes =
        match nodes with
        | [] -> `Leaf_visited state
        | hd :: tl ->
          match loop ~state ~node:hd with
          | `Candidate -> `Candidate
          | `Replaced node -> `Replaced (node :: tl)
          | `Leaf_visited i ->
            match loop_nodes ~state:i ~nodes:tl with
            | `Candidate -> `Candidate
            | `Replaced tl -> `Replaced (hd :: tl)
            | `Leaf_visited i -> `Leaf_visited i
      in
      match loop_nodes ~state:n ~nodes:root with
      | `Candidate -> None
      | `Leaf_visited i ->
        failwithf "Cannot visit leaf %d from in a tree with %d leaves"
          i n ()
      | `Replaced new_tree -> Some new_tree
    ;;
  
    let flip_several_leaves root indices =
      List.fold indices ~init:root ~f:flip_nth_leaf
    ;;
  
    let to_override_rules root =
      let rec loop_node ~source ~call_stack ~node =
        match node with
        | Declaration decl ->
          let call_stack =
            let closure = decl.closure in
            Call_site.Enter_decl { source; closure; } :: call_stack
          in
          List.concat_no_order (
            let source = Some decl.closure in
            List.map decl.children ~f:(fun node ->
              loop_node ~source ~call_stack ~node)
          )
  
        | Apply_inlined_function inlined ->
          let decision = true in
          let applied = inlined.applied in
          let offset = inlined.offset in
          let call_stack =
            Call_site.At_call_site { source; offset; applied } :: call_stack
          in
          let hd = { Data_collector.V0. call_stack; decision; applied } in
          hd :: List.concat_no_order (
            List.map inlined.children ~f:(fun node ->
              loop_node ~source:(Some inlined.applied) ~call_stack ~node)
          )
  
        | Apply_non_inlined_function not_inlined ->
          let decision = false in
          let applied = not_inlined.applied in
          let call_stack =
            let offset = not_inlined.offset in
            Call_site.At_call_site { source; offset; applied } :: call_stack
          in
          [{ call_stack; applied; decision }]
      in
      List.concat_map root ~f:(fun node ->
        loop_node ~source:None ~call_stack:[] ~node)
    ;;

    let to_simple_overrides root =
      Simple_overrides.of_v1_overrides (to_override_rules root)
    ;;
  
    let pp ppf (tree : root) =
      let mk_indent indent = String.make indent ' ' in
      let fprintf = Format.fprintf in
      let rec pp t ~indent =
        match (t : t) with
        | Declaration decl ->
          fprintf ppf "%s| Declaration { %s }\n"
            (mk_indent indent)
            (Caml.Format.asprintf "%a" Closure_id.print decl.closure);
          List.iter decl.children ~f:(fun a -> pp a ~indent:(indent + 1))
        | Apply_inlined_function inlined ->
          fprintf ppf "%s| [%d] Inlined { %s }\n"
            (mk_indent indent)
            (Call_site_offset.to_int inlined.offset)
            (Caml.Format.asprintf "%a" Closure_id.print inlined.applied);
          List.iter inlined.children ~f:(fun a -> pp a ~indent:(indent + 1))
        | Apply_non_inlined_function non_inlined ->
          fprintf ppf "%s| [%d] Not inlined { %s }\n"
            (mk_indent indent)
            (Call_site_offset.to_int non_inlined.offset)
            (Caml.Format.asprintf "%a" Closure_id.print non_inlined.applied)
      in
      List.iter tree ~f:(fun t -> pp t ~indent:0);
    ;;
  
    type t = root [@@deriving sexp, compare]
  end
  
  let fuzzy_equal a b =
    match a, b with
    | Declaration a, Declaration b ->
      Closure_id.equal a.closure b.closure
    | Apply_inlined_function a, Apply_inlined_function b ->
      Closure_id.equal a.applied b.applied &&
      Call_site.Offset.equal a.offset b.offset
    | Apply_non_inlined_function a, Apply_non_inlined_function b ->
      Closure_id.equal a.applied b.applied &&
      Call_site.Offset.equal a.offset b.offset
    | Apply_inlined_function a, Apply_non_inlined_function b ->
      Closure_id.equal a.applied b.applied &&
      Call_site.Offset.equal a.offset b.offset
    | _ , _ -> false

  let tag_and_function_almost_equal a b =
    match a, b with
    | Declaration a, Declaration b ->
      Closure_id.partial_equal a.closure b.closure
    | Apply_inlined_function a, Apply_inlined_function b ->
      Closure_id.partial_equal a.applied a.applied &&
      Call_site.Offset.equal a.offset b.offset
    | Apply_non_inlined_function a, Apply_non_inlined_function b ->
      Closure_id.partial_equal a.applied a.applied &&
      Call_site.Offset.equal a.offset b.offset
    | _ , _ -> false
  ;;
  
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
  
  let add (top_level_tree : Top_level.t) (collected : Data_collector.V0.t) =
  
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
          tree
          (* It is possible, due to the nature of the [inline] function. It
           * inlines the contents of the function body before trying to
           * deciding to inline the function itself.
           *
           * In cases as such, the parent takes higher priority (which should
           * contain an equally pessimistic or more pessimistic decision).
           *)
  
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
        tree
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
  
  let build (decisions : Data_collector.V0.t list) : Top_level.t =
    let (init : Top_level.t) = [] in
    List.fold decisions ~init ~f:add
  
  module Diff = struct
    type nonrec t =
      { common_ancestor : t list; (* Arbitary choice between the left and right *)
        left            : [ `Left of t  ] list;
        right           : [ `Right of t ] list;
      }
    [@@deriving sexp]
  end
  
  let diff ~(left : Top_level.t) ~(right : Top_level.t) =
    let shallow_diff ~left ~right =
      let same = ref [] in
      let left_only = ref [] in
      let right_only = ref [] in
      List.iter left ~f:(fun t ->
        if List.exists right ~f:(tag_and_function_almost_equal t) then
          same := (t, List.find_exn right ~f:(tag_and_function_almost_equal t)) :: !same
        else
          left_only := t :: !left_only);
      List.iter right ~f:(fun t ->
        if not (List.exists left ~f:(tag_and_function_almost_equal t)) then
          right_only := t :: !right_only;
      );
      (`Same !same, `Left_only !left_only, `Right_only !right_only)
    in
    let rec loop ~(left : t list) ~(right : t list) =
      let (`Same same, `Left_only left_only, `Right_only right_only) =
        shallow_diff ~left ~right
      in
      let descent =
        List.map same ~f:(fun (left, right) ->
          let diffs =
            match (left, right) with
            | Declaration l_decl, Declaration r_decl ->
              loop ~left:l_decl.children ~right:r_decl.children
            | Apply_inlined_function l_inlined, Apply_inlined_function r_inlined ->
              loop ~left:l_inlined.children ~right:r_inlined.children
            | Apply_non_inlined_function _, Apply_non_inlined_function _ ->
              []
            | _ -> assert false
          in
          List.map diffs ~f:(fun (diff : Diff.t) ->
            let common_ancestor = diff.common_ancestor in
            (* using [left] or [right] below is arbitrary *)
            { diff with common_ancestor = left :: common_ancestor }))
        |> List.concat_no_order
        |> List.filter ~f:(fun diff ->
            match diff.left, diff.right with
            | [], [] -> false
            | _, _ -> true)
      in
      let current =
        match left_only, right_only with
        | [], [] -> None
        | _, _ ->
          Some {
            Diff.
            left  = List.map left_only ~f:(fun x -> `Left x);
            right = List.map right_only ~f:(fun x -> `Right x);
            common_ancestor = [];
          }
      in
      match current with
      | Some hd -> hd :: descent
      | None -> descent
    in
    loop ~left ~right
  ;;
end

module V1 = struct

  module Function_metadata = Data_collector.V1.Function_metadata
  module Decision = Data_collector.V1.Decision
  module Overrides = Data_collector.V1.Overrides
  module Trace_item = Data_collector.V1.Trace_item
  module Action = Data_collector.V1.Action

  module T = struct
    type t =
      | Declaration of declaration
      | Apply_inlined_function of inlined_function
      | Apply_non_inlined_function of non_inlined_function
    and declaration =
      { declared  : Function_metadata.t;
        children  : t list;
      }
    and non_inlined_function =
      { applied   : Function_metadata.t;
        apply_id  : Apply_id.t;
      }
    and inlined_function =
      { applied   : Function_metadata.t;
        apply_id  : Apply_id.t;
        children  : t list;
      }
    [@@deriving sexp, compare]
  end
  
  include T
  include Comparable.Make(T)
  
  let is_leaf t =
    match t with
    | Apply_inlined_function { children = []; _ }
    | Declaration { children = []; _ }
    | Apply_non_inlined_function _ -> true
    | _ -> false
  ;;
  
  let flip_node t =
    match t with
    | Declaration _ -> assert false
    | Apply_inlined_function { applied; apply_id; children = _ } ->
      Apply_non_inlined_function { applied; apply_id; }
    | Apply_non_inlined_function { applied; apply_id; } ->
      Apply_inlined_function { applied; apply_id; children = [] }
  ;;
  
  let shallow_sexp_of_t sexp =
    let open Sexp in
    match sexp with
    | Declaration decl ->
      List [ Atom "Declaration"; Function_metadata.sexp_of_t decl.declared; ]
    | Apply_inlined_function inlined_function ->
      List [
        Atom "Apply_inlined_function";
        Function_metadata.sexp_of_t inlined_function.applied;
      ]
    | Apply_non_inlined_function non_inlined_function ->
      List [
        Atom "Apply_non_inlined_function";
        Function_metadata.sexp_of_t non_inlined_function.applied;
      ]

  let children t =
    match t with
    | Declaration decl -> decl.children
    | Apply_inlined_function inlined -> inlined.children
    | Apply_non_inlined_function _ -> []
  ;;
  
  let fuzzy_equal a b =
    match a, b with
    | Declaration a, Declaration b ->
      Function_metadata.equal a.declared b.declared
    | Apply_inlined_function a, Apply_inlined_function b ->
      Apply_id.equal a.apply_id b.apply_id

    | Apply_non_inlined_function a, Apply_non_inlined_function b ->
      Apply_id.equal a.apply_id b.apply_id

    | Apply_inlined_function a, Apply_non_inlined_function b ->
      Apply_id.equal a.apply_id b.apply_id

    | _ , _ -> false

  module Top_level = struct
    type root = t list [@@deriving sexp, compare]
  
    let count_leaves root =
      let rec loop t =
        let sum_all l =
          match l with
          | [] -> 1
          | l ->
            List.map ~f:loop l
            |> List.sum ~f:Fn.id (module Int)
        in
        match t with
        | Declaration decl -> sum_all decl.children
        | Apply_inlined_function inlined_function ->
          sum_all inlined_function.children
        | Apply_non_inlined_function _ -> 1
      in
      List.sum ~f:(fun t -> loop t) (module Int) root
    ;;
  
    let flip_nth_leaf root n =
      let rec loop ~state ~node =
        if is_leaf node then begin
          match state with
          | 0 -> `Replaced (flip_node node)
          | state -> `Leaf_visited (state - 1)
        end else begin
          match node with
          | Apply_non_inlined_function _ -> assert false
          | Declaration decl ->
            begin match loop_nodes ~state ~nodes:decl.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Replaced children ->
              `Replaced (Declaration { decl with children })
            end
          | Apply_inlined_function inlined ->
            begin match loop_nodes ~state ~nodes:inlined.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Replaced children ->
              `Replaced (
                Apply_inlined_function { inlined with children }
              )
            end
        end
      and loop_nodes ~state ~nodes =
        match nodes with
        | [] -> `Leaf_visited state
        | hd :: tl ->
          match loop ~state ~node:hd with
          | `Replaced node -> `Replaced (node :: tl)
          | `Leaf_visited i ->
            match loop_nodes ~state:i ~nodes:tl with
            | `Replaced tl -> `Replaced (hd :: tl)
            | `Leaf_visited i -> `Leaf_visited i
      in
      match loop_nodes ~state:n ~nodes:root with
      | `Leaf_visited i ->
        failwithf "Cannot visit leaf %d from in a tree with %d leaves"
          i n ()
      | `Replaced new_tree -> new_tree
    ;;
  
    let backtrack_nth_leaf root n =
      let rec loop ~state ~node =
        if is_leaf node then begin
          match state with
          | 0 -> `Candidate
          | state -> `Leaf_visited (state - 1)
        end else begin
          match node with
          | Apply_non_inlined_function _ -> assert false
          | Declaration decl ->
            begin match loop_nodes ~state ~nodes:decl.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Candidate -> `Candidate
            | `Replaced children ->
              `Replaced (Declaration { decl with children })
            end
          | Apply_inlined_function inlined ->
            begin match loop_nodes ~state ~nodes:inlined.children with
            | `Leaf_visited i -> `Leaf_visited i
            | `Candidate ->
              let applied = inlined.applied in
              let apply_id = inlined.apply_id in
              `Replaced (Apply_non_inlined_function { applied; apply_id; })
            | `Replaced children ->
              `Replaced (Apply_inlined_function { inlined with children })
            end
        end
      and loop_nodes ~state ~nodes =
        match nodes with
        | [] -> `Leaf_visited state
        | hd :: tl ->
          match loop ~state ~node:hd with
          | `Candidate -> `Candidate
          | `Replaced node -> `Replaced (node :: tl)
          | `Leaf_visited i ->
            match loop_nodes ~state:i ~nodes:tl with
            | `Candidate -> `Candidate
            | `Replaced tl -> `Replaced (hd :: tl)
            | `Leaf_visited i -> `Leaf_visited i
      in
      match loop_nodes ~state:n ~nodes:root with
      | `Candidate -> None
      | `Leaf_visited i ->
        failwithf "Cannot visit leaf %d from in a tree with %d leaves"
          i n ()
      | `Replaced new_tree -> Some new_tree
    ;;
  
    let flip_several_leaves root indices =
      List.fold indices ~init:root ~f:flip_nth_leaf
    ;;
  
    let to_override_rules root =
      let rec loop_node ~source ~call_stack ~node =
        match node with
        | Declaration decl ->
          let call_stack =
            let declared = decl.declared in
            Trace_item.Enter_decl { source; declared; } :: call_stack
          in
          List.concat_no_order (
            let source = Some decl.declared in
            List.map decl.children ~f:(fun node ->
              loop_node ~source ~call_stack ~node)
          )
  
        | Apply_inlined_function inlined ->
          let action = Action.Inline in
          let applied = inlined.applied in
          let apply_id = inlined.apply_id in
          let trace =
            Trace_item.At_call_site { source; apply_id; applied } :: call_stack
          in
          let call_stack = trace in
          let metadata = applied in
          let round = 0 in  (* TODO(fyq14): Support other rounds? *)
          let hd = { Decision. round; trace; action; metadata; apply_id } in
          hd :: List.concat_no_order (
            List.map inlined.children ~f:(fun node ->
              loop_node ~source:(Some inlined.applied) ~call_stack ~node)
          )
  
        | Apply_non_inlined_function not_inlined ->
          let applied = not_inlined.applied in
          let apply_id = not_inlined.apply_id in
          let trace =
            Trace_item.At_call_site { source; apply_id; applied } :: call_stack
          in
          let round = 0 in
          let metadata = applied in
          let action = Action.Apply in
          [{ Decision. round; trace; apply_id; action; metadata; }]
      in
      List.concat_map root ~f:(fun node ->
        loop_node ~source:None ~call_stack:[] ~node)
      |> Overrides.of_decisions
    ;;
  
    let pp ppf (tree : root) =
      let mk_indent indent = String.make indent ' ' in
      let fprintf = Format.fprintf in
      let rec pp t ~indent =
        match (t : t) with
        | Declaration decl ->
          fprintf ppf "%s| Declaration { %s }\n"
            (mk_indent indent)
            (Caml.Format.asprintf "%a" Function_metadata.print decl.declared);
          List.iter decl.children ~f:(fun a -> pp a ~indent:(indent + 1))
        | Apply_inlined_function inlined ->
          fprintf ppf "%s| [%a] Inlined { %s }\n"
            (mk_indent indent)
            Apply_id.print inlined.apply_id
            (Caml.Format.asprintf "%a" Function_metadata.print inlined.applied);
          List.iter inlined.children ~f:(fun a -> pp a ~indent:(indent + 1))
        | Apply_non_inlined_function non_inlined ->
          fprintf ppf "%s| [%a] Not inlined { %s }\n"
            (mk_indent indent)
            Apply_id.print non_inlined.apply_id
            (Caml.Format.asprintf "%a" Function_metadata.print non_inlined.applied)
      in
      List.iter tree ~f:(fun t -> pp t ~indent:0);
    ;;

    let pprint ?(indent = -1) buffer nodes =
      let rec loop ~indent node =
        let space = 
         List.init indent ~f:(fun _ -> " ")  |> String.concat ~sep:""
        in
        match node with
        | Declaration decl ->
          bprintf buffer "%sDECL(%s)\n" space
            (Format.asprintf "%a" Closure_origin.print decl.declared.closure_origin);
          iterate_children ~indent decl.children
        | Apply_inlined_function inlined ->
          bprintf buffer "%sINLINE[%s](%s)\n" space
            (Format.asprintf "%a" Apply_id.print inlined.apply_id)
            (Format.asprintf "%a" Closure_origin.print inlined.applied.closure_origin);
          iterate_children ~indent inlined.children
        | Apply_non_inlined_function non_inlined -> 
          bprintf buffer "%sDONT_INLINE(%s) %s\n" space
            (Format.asprintf "%a" Apply_id.print non_inlined.apply_id)
            (Format.asprintf "%a"
              Closure_origin.print non_inlined.applied.closure_origin);

      and iterate_children ~indent children =
        List.iter children ~f:(fun child ->
            loop ~indent:(indent + 1) child)
      in
      iterate_children ~indent:indent nodes
    ;;

    let rec is_super_tree ~super (tree : root) =
      List.for_all tree ~f:(fun tree_node ->
        match List.find super ~f:(fuzzy_equal tree_node) with
        | Some super_node -> is_super_node ~super:super_node tree_node
        | None -> false)

    and is_super_node ~(super : t)  (tree : t) =
      fuzzy_equal super tree
      && is_super_tree ~super:(children super) (children tree)
    ;;
 
    module T = struct
      type t = root [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make(T)
  end
  
  let equal_t_and_trace_item (t : t) (trace_item : Trace_item.t) =
    match (t, trace_item) with
    | (Declaration decl, Enter_decl enter_decl) ->
      Function_metadata.equal decl.declared enter_decl.declared
    | (Apply_inlined_function inlined_function, At_call_site at_call_site) ->
      Apply_id.equal inlined_function.apply_id at_call_site.apply_id
  
    | (Apply_non_inlined_function non_inlined_function, At_call_site at_call_site) ->
      Apply_id.equal non_inlined_function.apply_id at_call_site.apply_id
  
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
  
  let add (top_level_tree : Top_level.t) (collected : Decision.t) =
  
    let rec add__generic (tree : t) (call_stack : Trace_item.t list) =
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
        (enter_decl : Trace_item.enter_decl)
        (call_stack : Trace_item.t list) =
      match call_stack with
      | [] -> failwith "Declaration cannot be a leaf node"
      | otherwise ->
        let finder haystack =
          let default () =
            let declaration =
              { children = []; declared = enter_decl.declared; }
            in
            add__generic (Declaration declaration) otherwise
          in
          let f (child : t) =
            if equal_t_and_trace_item child
                (Trace_item.Enter_decl enter_decl)
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
          tree
          (* It is possible, due to the nature of the [inline] function. It
           * inlines the contents of the function body before trying to
           * deciding to inline the function itself.
           *
           * In cases as such, the parent takes higher priority (which should
           * contain an equally pessimistic or more pessimistic decision).
           *)
  
    and add__call_site
        (tree : t)
        (call_site : Trace_item.at_call_site)
        (call_stack : Trace_item.t list) =
      let finder haystack =
        match call_stack with
        | [] ->
          find_and_replace haystack
            ~default:(fun () ->
              match collected.action with
              | Action.Inline -> 
                let record =
                  { children = [];
                    apply_id = call_site.apply_id;
                    applied  = collected.metadata;
                  }
                in
                assert (
                  Function_metadata.equal collected.metadata call_site.applied
                );
                Apply_inlined_function record
              | Action.Apply ->
                let record =
                  { apply_id = call_site.apply_id;
                    applied  = collected.metadata;
                  }
                in
                assert (
                  Function_metadata.equal collected.metadata call_site.applied
                );
                Apply_non_inlined_function record)
            ~f:(fun (child : t) ->
              if equal_t_and_trace_item child
                (Trace_item.At_call_site call_site)
              then Some child
              else None)
        | otherwise ->
          find_and_replace haystack
            ~default:(fun () ->
              let record =
                { children = [];
                  apply_id   = call_site.apply_id;
                  applied  = call_site.applied;
                }
              in
              add__generic (Apply_inlined_function record) otherwise)
            ~f:(fun (child : t) ->
              if equal_t_and_trace_item child
                (Trace_item.At_call_site call_site)
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
        tree
    in
    let declared =
      let ident = Fyp_compiler_lib.Ident.create_persistent "<none>" in
      let linkage_name = Fyp_compiler_lib.Linkage_name.create "none" in
      let current_compilation_unit =
        Fyp_compiler_lib.Compilation_unit.create ident linkage_name
      in
      let variable =
        Fyp_compiler_lib.Variable.create ~current_compilation_unit
          "transient"
      in
      let closure_id = Closure_id.wrap variable in
      let closure_origin = Closure_origin.create closure_id in
      { Function_metadata.
        closure_id = Some closure_id;
        set_of_closures_id = None;
        closure_origin;
      }
    in
    let children = top_level_tree in
    let call_site = Declaration { declared ; children } in
    match
      add__generic call_site (List.rev collected.trace)
    with
    | Declaration { declared = _; children } -> children
    | _ -> assert false
  ;;

  let rec recursively_reverse children =
    let reverse_node node =
      match node with
      | Declaration decl ->
        Declaration { decl with children = recursively_reverse decl.children }
      | Apply_inlined_function inlined ->
        Apply_inlined_function {
          inlined with children = recursively_reverse inlined.children
        }
      | Apply_non_inlined_function _non_inlined -> node
    in
    List.rev_map children ~f:reverse_node

  let build (decisions : Data_collector.V1.Decision.t list) : Top_level.t =
    let (init : Top_level.t) = [] in
    let tree = List.fold decisions ~init ~f:add in
    recursively_reverse tree
  
  module Diff = struct
    type nonrec t =
      { common_ancestor : t list; (* Arbitary choice between the left and right *)
        left            : [ `Left of t  ] list;
        right           : [ `Right of t ] list;
      }
    [@@deriving sexp]
  end
  
  let diff ~(left : Top_level.t) ~(right : Top_level.t) =
    let shallow_diff ~left ~right =
      let same = ref [] in
      let left_only = ref [] in
      let right_only = ref [] in
      List.iter left ~f:(fun t ->
        if List.exists right ~f:(fuzzy_equal t) then
          same := t :: !same
        else
          left_only := t :: !left_only);
      List.iter right ~f:(fun t ->
        if not (List.exists left ~f:(fuzzy_equal t)) then
          right_only := t :: !right_only;
      );
      (`Same !same, `Left_only !left_only, `Right_only !right_only)
    in
    let rec loop ~(left : t list) ~(right : t list) =
      let (`Same same, `Left_only left_only, `Right_only right_only) =
        shallow_diff ~left ~right
      in
      let descent =
        let left =
          List.filter left ~f:(fun l -> List.exists same ~f:(fuzzy_equal l))
        in
        let right =
          List.filter right ~f:(fun r -> List.exists same ~f:(fuzzy_equal r))
        in
        List.map2_exn left right ~f:(fun l r ->
          let diffs =
            match l, r with
            | Declaration l_decl, Declaration r_decl ->
              loop ~left:l_decl.children ~right:r_decl.children
            | Apply_inlined_function l_inlined, Apply_inlined_function r_inlined ->
              loop ~left:l_inlined.children ~right:r_inlined.children
            | Apply_non_inlined_function _, Apply_non_inlined_function _ ->
              []
            | _, _ -> assert false
          in
          List.map diffs ~f:(fun (diff : Diff.t) ->
            let common_ancestor = diff.common_ancestor in
            { diff with common_ancestor = l :: common_ancestor }))
        |> List.concat_no_order
      in
      let current =
        match left, right with
        | [], [] -> None
        | _, _ ->
          Some {
            Diff.
            left  = List.map left_only ~f:(fun x -> `Left x);
            right = List.map right_only ~f:(fun x -> `Right x);
            common_ancestor = [];
          }
      in
      match current with
      | Some hd -> hd :: descent
      | None -> descent
    in
    loop ~left ~right
  ;;
end
