open Core
open Async
open Common

module RL = Rl

module Closure_id = Protocol.Shadow_fyp_compiler_lib.Closure_id
module Call_site = Protocol.Shadow_fyp_compiler_lib.Call_site

let zip_with_delay l =
  match l with
  | [] -> []
  | _ :: tl ->
    List.zip_exn l ((List.map ~f:(fun x -> Some x) tl) @ [None])
;;

module Function_call = struct
  module T = struct
    type t =
      { top_level_offset: Call_site_offset.t;
        call_stack: (Closure_id.t * Call_site_offset.t) list;
        applied:    Closure_id.t;
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make(T)

  let override_of_t t action =
    let call_stack =
      List.map (zip_with_delay t.call_stack)
        ~f:(fun ((source, offset), after)->
          let applied =
            match after with
            | None -> t.applied
            | Some (clos, _) -> clos
          in
          let source = Some source in
          let at_call_site = { Call_site. source; offset; applied; } in
          Call_site.At_call_site at_call_site
      )
    in
    let top_level_call_site =
      let source = None in
      let offset = t.top_level_offset in
      let applied =
        match t.call_stack with
        | [] -> t.applied
        | (clos, _) :: _ -> clos
      in
      let at_call_site = { Call_site. source; offset; applied; } in
      Call_site.At_call_site at_call_site
    in
    let call_stack = call_stack @ [top_level_call_site] in
    let applied = t.applied in
    let decision =
      match action with
      | RL.A.Inline -> true
      | RL.A.No_inline -> false
    in
    { Data_collector. call_stack; applied; decision; }
  ;;
end

type node =
  { inline:    RL.S.t;
    no_inline: RL.S.t;
  }

type t =
  { transitions: node RL.S.Map.t;
    root: RL.S.t;
    function_calls: Function_call.t RL.S.Map.t;
  }

let transition t clos action =
  let node = RL.S.Map.find_exn t.transitions clos in
  match action with
  | RL.A.Inline    -> node.inline
  | RL.A.No_inline -> node.no_inline
;;

let is_empty_list = function
  | [] -> true
  | _  -> false
;;

let overrides_of_pending_trajectory (t : t) (pending_trajectory : RL.Pending_trajectory.t) =
  List.map (fst pending_trajectory) ~f:(fun (s, a) ->
    let function_call = RL.S.Map.find_exn t.function_calls s in
    Function_call.override_of_t function_call a)
;;

let t_of_inlining_tree (top_level : Inlining_tree.Top_level.t) =
  let (top_level_ids, tree_map, call_map) =
    let (tree_map : RL.S.t list RL.S.Map.t ref) = ref RL.S.Map.empty in
    let (call_map : Function_call.t RL.S.Map.t ref) = ref RL.S.Map.empty in
    let rec loop tree_node ~call_stack ~foo =
      let update_map offset applied children =
        let id = RL.S.make () in
        let call_stack =
          match foo with
          | None -> call_stack
          | Some (_, source_function) ->
            (source_function, offset) :: call_stack
        in
        let (top_level_offset, function_call) =
          match foo with
          | Some (top_level_offset, closure_id) ->
            assert (not (is_empty_list call_stack));
            top_level_offset, { Function_call.
              call_stack;
              applied = applied;
              top_level_offset;
            }
          | None ->
            assert (is_empty_list call_stack);
            offset, { Function_call.
              call_stack;
              applied = applied;
              top_level_offset = offset;
            }
        in
        let children_ids =
          List.filter_map children ~f:(fun child_node ->
              loop child_node ~call_stack
                ~foo:(Some (top_level_offset, applied))
            )
        in
        call_map := RL.S.Map.add !call_map ~key:id ~data:function_call;
        tree_map := RL.S.Map.add !tree_map ~key:id ~data:children_ids;
        id
      in
      match (tree_node : Inlining_tree.t) with
      | Declaration _ -> None
      | Apply_non_inlined_function non_inlined ->
        Some (update_map non_inlined.offset non_inlined.applied [])
      | Apply_inlined_function inlined ->
        Some (update_map inlined.offset inlined.applied inlined.children)
    in
    let top_level_ids =
      List.filter_map top_level ~f:(fun tree_node ->
          loop tree_node ~call_stack:[] ~foo:None)
    in
    (top_level_ids, !tree_map, !call_map)
  in
  let rec loop ~(acc : node RL.S.Map.t) ~(tree_node : RL.S.t) ~backtrack =
    let child_nodes = RL.S.Map.find_exn tree_map tree_node in
    let inline =
      match child_nodes with
      | [] -> backtrack
      | hd :: _ -> hd
    in
    let no_inline = backtrack in
    let data = { inline; no_inline; } in
    let acc = RL.S.Map.add acc ~key:tree_node ~data in
    List.fold_left (zip_with_delay child_nodes) ~init:acc
      ~f:(fun acc (child, maybe_next) ->
          let backtrack =
            match maybe_next with
            | None -> backtrack
            | Some x -> x
          in
          loop ~acc ~tree_node:child ~backtrack)
  in
  let transitions =
    List.fold_left (zip_with_delay top_level_ids) ~init:RL.S.Map.empty
      ~f:(fun acc (top_level_id, next_id) ->
          let backtrack =
            Option.value ~default:RL.S.terminal next_id
          in
          loop ~acc ~tree_node:top_level_id ~backtrack)
  in
  let root = List.hd_exn top_level_ids in
  { transitions; root; function_calls = call_map; }
;;

let pprint ?(with_legend: unit option) t =
  RL.S.Map.to_alist t.transitions
  |> List.map ~f:(fun (s, node) ->
      sprintf !"[%{RL.S}] (%{RL.S}, %{RL.S})"
        s node.inline node.no_inline)
  |> String.concat ~sep:"\n"
;;
