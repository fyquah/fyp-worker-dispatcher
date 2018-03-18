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
  ;;

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

  exception Flip_error of string

  let to_identifier = function
    | Declaration { declared; _ }  ->
      Format.asprintf "{%a}" Closure_origin.print declared.closure_origin
    | Apply_inlined_function { applied; apply_id; _ } ->
      Format.asprintf "<%a(%a)>"
        Closure_origin.print applied.closure_origin
        Apply_id.print apply_id
    | Apply_non_inlined_function { applied; apply_id; _ } ->
      Format.asprintf "<%a(%a)|>"
        Closure_origin.print applied.closure_origin
        Apply_id.print apply_id
  ;;

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
      if Closure_origin.equal applied.closure_origin Closure_origin.unknown then begin
        raise (Flip_error "Cannot flip an unknown node")
      end;
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

  let locally_unique_identifier = function
    | Declaration a -> `Declaration a.declared
    | Apply_inlined_function a -> `Application a.apply_id
    | Apply_non_inlined_function a -> `Application a.apply_id
  ;;

  let closure_origin_weak_equal a b =
    let closure_origin_get_weak_repr a =
      (Closure_origin.get_compilation_unit a, Closure_origin.get_name a)
    in
    let a = closure_origin_get_weak_repr a in
    let b = closure_origin_get_weak_repr b in
    Compilation_unit.equal (fst a) (fst b)
      && String.equal (snd a) (snd b)
  ;;

  let weak_equal_disregarding_children a b =
    match a, b with
    | Declaration a, Declaration b ->
       closure_origin_weak_equal
         a.declared.closure_origin b.declared.closure_origin
    | Apply_inlined_function a, Apply_inlined_function b ->
      Apply_id.equal_accounting_deprecation a.apply_id b.apply_id

    | Apply_non_inlined_function a, Apply_non_inlined_function b ->
      Apply_id.equal_accounting_deprecation a.apply_id b.apply_id

    | _ , _ -> false
  ;;

  let equal_disregarding_children a b =
    match a, b with
    | Declaration a, Declaration b ->
      Function_metadata.equal a.declared b.declared
    | Apply_inlined_function a, Apply_inlined_function b ->
      Apply_id.equal_accounting_deprecation a.apply_id b.apply_id

    | Apply_non_inlined_function a, Apply_non_inlined_function b ->
      Apply_id.equal_accounting_deprecation a.apply_id b.apply_id

    | _ , _ -> false
  ;;

  let refers_to_same_node a b =
    match (locally_unique_identifier a), (locally_unique_identifier b) with
    | `Declaration a, `Declaration b ->
      Closure_origin.equal a.closure_origin b.closure_origin
    | `Application a, `Application b ->
      Apply_id.equal_accounting_deprecation a b
    | _, _ -> false
  ;;

  let weak_refers_to_same_node a b =
    match (locally_unique_identifier a), (locally_unique_identifier b) with
    | `Declaration a, `Declaration b ->
      closure_origin_weak_equal a.closure_origin b.closure_origin
    | `Application a, `Application b ->
      Apply_id.equal_accounting_deprecation a b
    | _, _ -> false
  ;;

  module Top_level = struct
    type node = t

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
      let rec loop ~state ~(node : t) =
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
      try
        begin match loop_nodes ~state:n ~nodes:root with
        | `Leaf_visited i ->
          failwithf "Cannot visit leaf %d from in a tree with %d leaves"
            i n ()
        | `Replaced new_tree -> Some new_tree
        end
      with
      | Flip_error _ -> None
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
      List.fold indices ~init:(Some root) ~f:(fun b idx ->
          Option.bind b ~f:(fun a -> flip_nth_leaf a idx))
    ;;

    let unique_random_from_list ~count choices =
      let rec loop ~choices ~(left : int) =
        if Int.O.(left <= 0) then []
        else begin
          let size = Int.Set.length choices in
          let selected =
            Option.value_exn (Int.Set.nth choices (Random.int size))
          in
          selected :: loop ~left:(left - 1) ~choices:(Int.Set.remove choices selected)
        end
      in
      loop ~choices:(Int.Set.of_list choices) ~left:count
    ;;

    let rec uniform_random_mutation tree =
      let num_leaves = count_leaves tree in
      if Random.bool () then
        let modified_leaves = 1 in
        let choices =
          unique_random_from_list ~count:modified_leaves
            (List.init num_leaves ~f:Fn.id)
        in
        Log.Global.sexp ~level:`Info [%message "Flipping leaf node!"];
        match flip_several_leaves tree choices with
        | None -> uniform_random_mutation tree
        | Some t ->
          Log.Global.sexp ~level:`Info [%message
            "Flipping node" (choices : int list)"succeeded"];
        t
      else
        let leaf = Random.int num_leaves in
        Log.Global.sexp ~level:`Info [%message "Attempting backtrack!"];
        match backtrack_nth_leaf tree leaf with
        | None -> uniform_random_mutation tree
        | Some transformed ->
          Log.Global.sexp ~level:`Info [%message "Back track possible!!"];
          transformed
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
      |> List.filter ~f:(fun decision ->
          not (Closure_origin.equal decision.metadata.closure_origin
                 Closure_origin.unknown))
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

    let node_children = function
      | Declaration { children; _ }
      | Apply_inlined_function { children; _ } -> children
      | Apply_non_inlined_function _ -> []
    ;;

    let rec
      size_top_level l = List.fold l ~init:0 ~f:(fun b a -> b + size a)
    and size = function
      | Declaration { children; _ }
      | Apply_inlined_function { children; _ } -> 1 + size_top_level children
      | Apply_non_inlined_function _ -> 1
    ;;

    module Soundness = struct
      type t = {
        is_sound:                 bool;
        matched_reference_nodes:  int;
        total_nodes_in_reference: int;
      }
    end

    let rec remove_empty_declarations root =
      List.filter_map root ~f:(fun node ->
          match node with
          | Declaration decl ->
            let children = remove_empty_declarations decl.children in
            if Int.equal (List.length children) 0 then
              None
            else
              Some (Declaration { decl with children })

          | Apply_inlined_function inlined ->
            let children = remove_empty_declarations inlined.children in
            Some (Apply_inlined_function { inlined with children })

          | Apply_non_inlined_function _ -> Some node)
    ;;

    let rec remove_unknowns root =
      List.filter_map root ~f:(fun node ->
          match node with
          | Declaration decl ->
            let children = remove_unknowns decl.children in
            Some (Declaration { decl with children })

          | Apply_inlined_function inlined ->
            let children = remove_unknowns inlined.children in
            Some (Apply_inlined_function { inlined with children })

          | Apply_non_inlined_function { applied; _ } ->
            if Closure_origin.equal Closure_origin.unknown
               applied.closure_origin
            then None
            else Some node)
    ;;

    let check_soundness ?loose ~reference:reference_tree ~compiled:compiled_tree () =
      let matched_reference_nodes = ref 0 in
      let refers_to_same_node =
        match loose with
        | Some () ->  weak_refers_to_same_node
        | None -> refers_to_same_node
      in
      let rec loop ~reference ~compiled =
        List.for_all compiled ~f:(fun compiled_node ->
            let matching_node_in_reference =
              List.find_map reference ~f:(fun reference_node ->
                  if refers_to_same_node compiled_node reference_node then begin
                    match compiled_node, reference_node with
                    | Declaration _, Declaration _
                    | Apply_inlined_function _, Apply_inlined_function _
                    | Apply_non_inlined_function _, Apply_non_inlined_function _ ->
                      Some (`Matched reference_node)
                    | _ -> Some (`Failed)
                  end else begin
                    None
                  end)
            in
            match matching_node_in_reference with
            | None -> true
            | Some `Failed -> false
            | Some (`Matched reference_node) ->
              matched_reference_nodes := !matched_reference_nodes + 1;
              loop ~reference:(node_children reference_node)
                ~compiled:(node_children compiled_node))
      in
      let is_sound =
        loop ~reference:reference_tree ~compiled:compiled_tree
      in
      let matched_reference_nodes = !matched_reference_nodes in
      let total_nodes_in_reference = size_top_level reference_tree in
      { Soundness.
        is_sound;
        matched_reference_nodes;
        total_nodes_in_reference;
      }
    ;;

    let to_simple_overrides root =
      Data_collector.Simple_overrides.of_v1_overrides (to_override_rules root)
    ;;

    module Expanded = struct
      type decl = {
        func : Function_metadata.t;
        children : node list;
      }
      and inlined = {
        func     : Function_metadata.t;
        path     : Apply_id.Path.t;
        children : node list;
      }
      and apply = {
        func : Function_metadata.t;
        path : Apply_id.Path.t;
      }
      and node =
        | Decl    of decl
        | Inlined of inlined
        | Apply   of apply
      [@@deriving sexp]

      let node_equal_without_descend a b =
        match a, b with
        | Decl a, Decl b ->
          Closure_origin.equal a.func.closure_origin b.func.closure_origin
        | Inlined a, Inlined b -> Apply_id.Path.equal a.path b.path
        | Apply   a, Apply   b -> Apply_id.Path.equal a.path b.path
        | _ -> false
      ;;

      let get_children_exn node =
        match node with
        | Decl decl -> decl.children
        | Inlined inlined -> inlined.children
        | Apply _ -> assert false
      ;;

      let get_children_with_empty_default node =
        match node with
        | Decl decl -> decl.children
        | Inlined inlined -> inlined.children
        | Apply _ -> []
      ;;

      let update_children_exn node children =
        match node with
        | Decl decl -> Decl { decl with children }
        | Inlined inlined -> Inlined { inlined with children }
        | Apply _ -> assert false
      ;;

      let rec weak_equal a b =
        List.equal a b ~equal:(fun a b ->
            node_equal_without_descend a b && (
              weak_equal
                (get_children_with_empty_default a)
                (get_children_with_empty_default b)))
      ;;

      let pprint ?(indent = -1) buffer nodes =
        let rec loop ~indent node =
          let space =
           List.init indent ~f:(fun _ -> " ")  |> String.concat ~sep:""
          in
          match node with
          | Decl decl ->
            bprintf buffer "%sDECL(%s)\n" space
              (Format.asprintf "%a" Closure_origin.print decl.func.closure_origin);
            iterate_children ~indent decl.children
          | Inlined inlined ->
            bprintf buffer "%sINLINE[%s](%s)\n" space
              (Apply_id.Path.to_string inlined.path)
              (Format.asprintf "%a"
                 Closure_origin.print inlined.func.closure_origin);
            iterate_children ~indent inlined.children
          | Apply apply ->
            bprintf buffer "%sAPPLY[%s](%s)\n" space
              (Apply_id.Path.to_string apply.path)
              (Format.asprintf "%a"
                Closure_origin.print apply.func.closure_origin);

        and iterate_children ~indent children =
          List.iter children ~f:(fun child ->
              loop ~indent:(indent + 1) child)
        in
        iterate_children ~indent:indent nodes
      ;;

      let rec filter_map root ~f =
        let open Option.Let_syntax in
        List.filter_map root ~f:(fun (node : node) ->
            let%map node = f node in
            match node with
            | Decl decl ->
              let children = filter_map ~f decl.children in
              Decl { decl with children }
            | Inlined inlined ->
              let children = filter_map ~f inlined.children in
              Inlined { inlined with children }
            | Apply _ -> node
          )
      ;;

      type t = node list [@@deriving sexp]

      let of_list (t : t) = t

      let rec add_node_to_parent_head (root : t) (arg_node : node) =
        let matches_arg = node_equal_without_descend arg_node in
        match List.find root ~f:matches_arg with
        | None -> arg_node :: root
        | Some matched_node_in_root ->
          let children_of_matched_node_in_root =
            get_children_exn matched_node_in_root
          in
          let children_of_matched_node_in_root =
            List.fold (get_children_with_empty_default arg_node)
              ~init:children_of_matched_node_in_root
              ~f:add_node_to_parent_head
          in
          let new_arg_node =
            update_children_exn matched_node_in_root
              children_of_matched_node_in_root
          in
          List.map root ~f:(fun child_node ->
              if matches_arg child_node then
                new_arg_node
              else
                child_node)
      ;;

      let expanded_function_metadata =
        let closure_origin =
          let current_compilation_unit =
            let ident = Fyp_compiler_lib.Ident.create_persistent "expanded" in
            let linkage_name = Fyp_compiler_lib.Linkage_name.create "expanded" in
            Compilation_unit.create ident linkage_name
          in
          Fyp_compiler_lib.Closure_origin.create (
            Fyp_compiler_lib.Closure_id.wrap (
              Fyp_compiler_lib.Variable.create ~current_compilation_unit "expanded"))
        in
        { Function_metadata.
          closure_id = None; set_of_closures_id = None;
          opt_closure_origin = None;
          closure_origin;
          specialised_for = None;
        }
      ;;
    end

    module E = Expanded

    let rec map ~f root =
      let add_children node children =
        match node with
        | Declaration d -> Declaration { d with children }
        | Apply_inlined_function inlined ->
          Apply_inlined_function { inlined with children }
        | Apply_non_inlined_function _ -> node
      in
      List.map root ~f:(fun node ->
        match node with
        | Declaration d ->
          let new_node = (f node) in
          let children = map ~f d.children in
          add_children new_node children

        | Apply_inlined_function inlined ->
          let new_node = (f node) in
          let children = map ~f inlined.children in
          add_children new_node children

        | Apply_non_inlined_function _ -> f node)
    ;;

    let expand_decisions_in_call_site_based_on_inlining_path input_tree =
      let rec trim_prefix ~prefix path =
        match prefix, path with
        | [], path -> path
        | prefix_hd :: prefix_tl, hd :: tl ->
          if not (Apply_id.Node_id.equal prefix_hd hd) then begin
            failwithf "prefix don't match! prefix=(%s) and (%s)"
              (Apply_id.Path.to_string prefix)
              (Apply_id.Path.to_string path)
              ();
          end;
          trim_prefix ~prefix:prefix_tl tl
        | _, [] -> assert false
      in
      let remove_last_node_exn l =
        List.rev (List.tl_exn (List.rev l))
      in
      let expand_inlining_path_parents
          ~child_node
          ~history
          ~inlining_path:this_inlining_path =
        assert (Int.(>) (List.length this_inlining_path) 0);
        (* Recall that the path is from top to bottom, that is if
         *   [f_a] -> [f_b] -> [f_c] -> [f_d]
         * This is /a/b/c/d
         *)
        let path_prefix = history in
        let path_to_patch =
          remove_last_node_exn (
            trim_prefix ~prefix:path_prefix this_inlining_path)
        in
        (* We construct the node bottom-up to prevent stack-overflow
         * (this recursive depth can be potentially very very deep) *)
        let rec patch_bottom_up ~child ~rev_path =
          match rev_path with
          | [] -> child
          | _ :: tl ->
            let inlined = 
              { E.
                func     = E.expanded_function_metadata;
                path     = history @ List.rev rev_path;
                children = [ child ];
              }
            in
            let child = E.Inlined inlined in
            patch_bottom_up ~child ~rev_path:tl
        in
        patch_bottom_up ~child:child_node ~rev_path:(List.rev path_to_patch)
      in
      let rec loop ~history root =
        List.map root ~f:(function
            | Declaration decl ->
              let children = loop ~history:[] decl.children in
              E.Decl { 
                children = children;
                func     = decl.declared;
              }

            | Apply_inlined_function inlined ->
              let inlining_path = Apply_id.to_path inlined.apply_id in
              let children = loop ~history:inlining_path inlined.children in
              Log.Global.sexp ~level:`Info
                [%message (inlining_path : Apply_id.Path.t) (history: Apply_id.Path.t)];
              let child_node =
                E.Inlined {
                  func     = inlined.applied;
                  path     = inlining_path;
                  children = children;
                }
              in
              expand_inlining_path_parents ~child_node ~history ~inlining_path

            | Apply_non_inlined_function non_inlined ->
              let inlining_path = Apply_id.to_path non_inlined.apply_id in
              let child_node =
                E.Apply {
                  func     = non_inlined.applied;
                  path     = inlining_path;
                }
              in
              expand_inlining_path_parents ~child_node ~history ~inlining_path)
      in
      loop ~history:[] input_tree
    ;;

    let rec merge_decisions_in_call_site (tree : Expanded.t) =
      let used = Array.create ~len:(List.length tree) false in
      let tree = Array.of_list tree in

      Array.mapi tree ~f:(fun i this_node ->
          if used.(i) then
            None
          else begin
            let additional_children =
              let ret = ref [] in
              for j = i + 1 to Array.length tree - 1 do
                if not used.(j) then begin
                  let another_node = tree.(j) in
                  if
                    Expanded.node_equal_without_descend another_node this_node
                  then begin
                    used.(j) <- true;
                    ret := another_node :: !ret
                  end
                end
              done;
              List.rev !ret
              |> List.concat_map ~f:(function
                  | E.Inlined { children; _ } -> children
                  | E.Decl { children; _ } -> children
                  | E.Apply _ -> [])
            in
            used.(i) <- true;
            match additional_children with
            | [] -> Some this_node
            | additional_children ->
              Some (
                E.update_children_exn this_node (
                  E.get_children_exn this_node @ additional_children))
          end)
      |> Array.to_list
      |> List.filter_opt
      |> List.map ~f:(function
          | E.Inlined inlined ->
            let children = merge_decisions_in_call_site inlined.children in
            E.Inlined { inlined with children }
          | E.Decl decl ->
            let children = merge_decisions_in_call_site decl.children in
            E.Decl { decl with children }
          | E.Apply a -> E.Apply a)
    ;;

    (*
     * The input has the form
     *
     * {F}:
     *    <inline:A>
     *      <noline:B>
     *    <inline:C>
     *
     * <inline:F>
     *   <inline:A>
     *     <inline:B>
     *
     * We want to transform it to:
     *
     * <inline:F>
     *   <inline:A>
     *     <inline:B>
     *   <inline:C>
     *
     * to denote that the inlining decision C exists.
     *
     * TODO: The following implementation jumbles up the order, this is 
     *       not ideal, but not catostrophic..
     *)
    let fill_in_decisions_from_declaration input_root =
      let declaration_map = ref Closure_id.Map.empty in
      let replay_declaration_inlined_leaves inlined_root
          ~inlining_path ~declaration_root =
        let nodes_to_replay =
          E.filter_map declaration_root ~f:(fun node ->
              match node with
              | E.Decl   _ -> None
              | E.Inlined inlined ->
                let path = inlining_path @ inlined.path in
                Some (E.Inlined { inlined with path })
              | E.Apply _ ->  None)
          |> List.rev
        in
        List.fold nodes_to_replay ~init:inlined_root ~f:(fun root node ->
            E.add_node_to_parent_head root node)
      in
      let rec loop root =
        List.map root ~f:(function
            | E.Decl decl ->
              let children = loop decl.children in
              let decl = { E. children = children; func = decl.func } in
              let closure_id =
                let message =
                  Format.asprintf "%a" Closure_origin.print
                    decl.func.closure_origin
                in
                Option.value_exn ~message decl.func.closure_id
              in
              declaration_map := (
                  Closure_id.Map.add closure_id decl !declaration_map);
              E.Decl decl

            | E.Inlined inlined ->
              let open Option.Let_syntax in 
              let new_children =
                let%bind closure_id = inlined.func.closure_id in
                let%map decl =
                  Closure_id.Map.find_opt closure_id !declaration_map
                in
                replay_declaration_inlined_leaves inlined.children
                  ~inlining_path:inlined.path
                  ~declaration_root:decl.children
              in
              let new_children =
                Option.value new_children ~default:inlined.children
              in
              let new_children = loop new_children in
              E.Inlined { inlined with children = new_children }

            | E.Apply apply -> E.Apply apply)
      in
      loop input_root
    ;;

    (* We are not relabelling -- okay? **)
    let expand root =
      root
      |> expand_decisions_in_call_site_based_on_inlining_path
      |> merge_decisions_in_call_site
      |> fill_in_decisions_from_declaration
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
              | Action.Specialise -> assert false
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
        opt_closure_origin = None;
        specialised_for = None;
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
  ;;

  let build (decisions : Data_collector.V1.Decision.t list) : Top_level.t =
    let (init : Top_level.t) = [] in
    List.filter decisions ~f:(fun d ->
        match d.action with
        | Data_collector.Action.Specialise -> false
        | Data_collector.Action.Inline 
        | Data_collector.Action.Apply -> true)
    |> List.fold ~init ~f:add
    |> recursively_reverse
  ;;

  module Diff = struct
    type nonrec t =
      { common_ancestor : t list; (* Arbitary choice between the left and right *)
        left            : [ `Left of t  ] list;
        right           : [ `Right of t ] list;
      }
    [@@deriving sexp]
  end

  let diff ?loose ~(left : Top_level.t) ~(right : Top_level.t) =
    let equal_disregarding_children =
      match loose with
      | None -> equal_disregarding_children
      | Some () -> weak_equal_disregarding_children
    in
    let shallow_diff ~left ~right =
      let same = ref [] in
      let left_only = ref [] in
      let right_only = ref [] in
      List.iter left ~f:(fun t ->
        if List.exists right ~f:(equal_disregarding_children t) then
          same := (t, List.find_exn right ~f:(equal_disregarding_children t)) :: !same
        else
          left_only := t :: !left_only);
      List.iter right ~f:(fun t ->
        if not (List.exists left ~f:(equal_disregarding_children t)) then
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
            match left, right with
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
            { diff with common_ancestor = left :: common_ancestor }))
        |> List.concat_no_order
        |> List.filter ~f:(fun diff ->
            match diff.left, diff.right with
            | [], [] -> false
            | _, _ -> true)
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
