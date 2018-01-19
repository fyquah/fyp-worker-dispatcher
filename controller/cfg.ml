open Core
open Async
open Common

type node =
  { inline: Closure_id.t;
    no_inline: Closure_id.t;
  }

type t =
  { node_map: node Closure_id.Map.t;
    root: Closure_id.t;
  }

let transition t clos action =
  Option.map (Closure_id.Map.find_opt t.node_map) ~f:(fun node ->
      match action with
      | RL.A.Inline    -> node.inline
      | RL.A.No_inline -> node.no_inline)
;;
