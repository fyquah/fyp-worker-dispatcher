open Core
open Async
open Common
open Data_collector.V1

module RL = Rl

module Closure_id = Protocol.Shadow_fyp_compiler_lib.Closure_id
module Call_site = Protocol.Shadow_fyp_compiler_lib.Call_site

let zip_with_delay l =
  match l with
  | [] -> []
  | _ :: tl ->
    List.zip_exn l ((List.map ~f:(fun x -> Some x) tl) @ [None])
;;


(* TODO(fyq14): This handles only function calls from the top-level, need
 *              to handle function calls inside declarations as well.
 *)
module Function_call = struct
  module T = struct
    type t = 
      { inlining_trace: (Apply_id.t * Function_metadata.t) list;
        apply_id      : Apply_id.t;
        applied       : Function_metadata.t;
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make(T)

  let t_of_decision (decision : Decision.t) =
    let trace = decision.trace in
    if
      List.for_all trace ~f:(function
          | Trace_item.Enter_decl _ -> false
          | Trace_item.At_call_site _ ->  true)
    then begin
      begin match List.hd_exn trace with
      | Trace_item.Enter_decl _ -> assert false
      | At_call_site acs ->
        assert (
          Apply_id.equal decision.apply_id acs.apply_id
          && Function_metadata.equal decision.metadata acs.applied
        )
      end;
      let inlining_trace =
        List.tl_exn trace
        |> List.map ~f:(function
            | Trace_item.Enter_decl _ -> assert false
            | Trace_item.At_call_site acs -> acs)
        |> List.map ~f:(fun (acs : Trace_item.at_call_site) ->
            (acs.apply_id, acs.applied))
      in
      let apply_id = decision.apply_id in
      let applied = decision.metadata in
      Some { inlining_trace; apply_id; applied; }
    end else begin
      None
    end
  ;;

  let decision_of_t t action =
    let apply_id = t.apply_id in
    let round = 0 in
    let trace =
      List.map (zip_with_delay t.inlining_trace)
        ~f:(fun ((apply_id, applied), source)->
          let source = Option.map ~f:snd source in
          Trace_item.At_call_site { source; apply_id; applied })
    in
    let metadata = t.applied in
    { Decision. round; trace; metadata; apply_id; action; }
  ;;
end

type node =
  { inline:    RL.S.t;
    no_inline: RL.S.t;
  }
[@@deriving sexp]

type t =
  { transitions: node RL.S.Map.t;
    root: RL.S.t;
    function_calls: Function_call.t RL.S.Map.t;
    reverse_map: RL.S.t Function_call.Map.t;
  }
[@@deriving sexp]

let transition t clos action =
  let node = RL.S.Map.find_exn t.transitions clos in
  match action with
  | RL.A.Inline  -> node.inline
  | RL.A.Apply   -> node.no_inline
;;

let is_empty_list = function
  | [] -> true
  | _  -> false
;;

let overrides_of_pending_trajectory (t : t) (pending_trajectory : RL.Pending_trajectory.t) =
  List.map (fst pending_trajectory) ~f:(fun (s, a) ->
    let function_call = RL.S.Map.find_exn t.function_calls s in
    Function_call.decision_of_t function_call a)
  |> Overrides.of_decisions
;;

let t_of_inlining_tree (top_level : Inlining_tree.V1.Top_level.t) =
  let (top_level_ids, tree_map, call_map) =
    let (tree_map : RL.S.t list RL.S.Map.t ref) = ref RL.S.Map.empty in
    let (call_map : Function_call.t RL.S.Map.t ref) = ref RL.S.Map.empty in

    let rec loop tree_node ~trace =
      let update_map apply_id applied children =
        let id = RL.S.make () in
        let inlining_trace = trace in
        let function_call =
          { Function_call.
            inlining_trace;
            applied = applied;
            apply_id = apply_id;
          }
        in
        let children_ids =
          List.filter_map children ~f:(fun child_node ->
              loop child_node ~trace:(
                (apply_id, applied) :: inlining_trace))
        in
        call_map := RL.S.Map.add !call_map ~key:id ~data:function_call;
        tree_map := RL.S.Map.add !tree_map ~key:id ~data:children_ids;
        id
      in
      match (tree_node : Inlining_tree.V1.t) with
      | Declaration _ -> None
      | Apply_non_inlined_function non_inlined ->
        Some (update_map non_inlined.apply_id non_inlined.applied [])
      | Apply_inlined_function inlined ->
        Some (update_map inlined.apply_id inlined.applied inlined.children)
    in
    let top_level_ids =
      List.filter_map top_level ~f:(fun tree_node ->
          loop tree_node ~trace:[])
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
  let reverse_map =
    RL.S.Map.to_alist call_map
    |> List.map ~f:(fun (a, b) -> (b, a))
    |> Function_call.Map.of_alist_exn
  in
  { transitions; root; function_calls = call_map; reverse_map; }
;;

let pprint ?(with_legend: unit option) t =
  RL.S.Map.to_alist t.transitions
  |> List.map ~f:(fun (s, node) ->
      sprintf !"[%{RL.S.pprint}] (%{RL.S.pprint}, %{RL.S.pprint})"
        s node.inline node.no_inline)
  |> String.concat ~sep:"\n"
;;
