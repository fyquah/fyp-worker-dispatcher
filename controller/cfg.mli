open Common

(* Builds a control flow graph of inlining decisions in the entire
 * source tree.
 *)

module RL = Rl

type node =
  { inline: Closure_id.t option;
    no_inline: Closure_id.t option;
  }

type t = private
  { node_map: node Closure_id.Map.t;
    root: Closure_id.t;
  }

val t_of_inlining_tree : Inlining_tree.Top_level.t -> t

val transition : t -> Closure_id.t -> RL.A.t -> Closure_id.t option
