open Core
open Async
open Common

module RL = Rl

type node =
  { inline: Closure_id.t option;
    no_inline: Closure_id.t option;
  }

type t =
  { node_map: node Closure_id.Map.t;
    root: Closure_id.t;
  }

let transition t clos action =
  Option.bind (Closure_id.Map.find_opt clos t.node_map) ~f:(fun node ->
      match action with
      | RL.A.Inline    -> node.inline
      | RL.A.No_inline -> node.no_inline)
;;


let t_of_inlining_tree (top_level : Inlining_tree.Top_level.t) =
  let filter_children_ids children =
    List.filter_map children ~f:(function
        | Inlining_tree.Declaration _ -> None
        | Apply_inlined_function a -> Some a.applied
        | Apply_non_inlined_function a -> Some a.applied)
  in
  let children_lookup =
    let rec loop tree_node ~acc =
      let update_map clos children =
        let children_ids = filter_children_ids children in
        let init = Closure_id.Map.add clos children_ids acc in
        List.fold_left children ~init ~f:(fun b a -> 
            loop tree_node ~acc:b)
      in
      match (tree_node : Inlining_tree.t) with
      | Declaration _ -> acc
      | Apply_non_inlined_function non_inlined ->
        update_map non_inlined.applied []
      | Apply_inlined_function inlined ->
        update_map inlined.applied inlined.children
    in
    List.fold_left top_level ~init:Closure_id.Map.empty
        ~f:(fun acc tree_node -> loop tree_node ~acc)
  in
  let rec loop
      ~(acc : node Closure_id.Map.t)
      ~(tree_node : (Closure_id.t * Closure_id.t list))
      ~next_stack =
    let backtracked =
      match next_stack with
      | hd :: _ -> Some hd
      | _ -> None
    in
    let inline =
      match snd tree_node with
      | [] -> backtracked
      | hd :: _ -> Some hd
    in
    let no_inline = backtracked in
    let key = fst tree_node in
    let data = { inline; no_inline; } in
    let acc = Closure_id.Map.add key data acc in
    List.fold_left (snd tree_node) ~init:acc ~f:(fun b key ->
        match Closure_id.Map.find_opt key children_lookup with
        | Some children ->
          let tree_node = (key, children) in
          loop ~acc:b ~tree_node ~next_stack
        | None ->
          let tree_node = (key, []) in
          loop ~acc:b ~tree_node ~next_stack)
  in
  let children_ids = filter_children_ids top_level in
  let node_map =
    List.fold_left children_ids ~init:Closure_id.Map.empty
      ~f:(fun b clos_id ->
          let tree_node =
            (clos_id, Closure_id.Map.find clos_id children_lookup)
          in
          let next_stack = [] in
          loop ~acc:b ~tree_node ~next_stack)
  in
  let root = List.hd_exn children_ids in
  { node_map; root; }
;;
