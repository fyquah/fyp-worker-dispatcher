open Common

(* Builds a control flow graph of inlining decisions in the entire
 * source tree.
 *)

module RL = Rl

type node =
  { inline: RL.S.t option;
    no_inline: RL.S.t option;
  }

type t = private
  { node_map: node RL.S.Map.t;
    root:          RL.S.t;
  }

val t_of_inlining_tree : Inlining_tree.Top_level.t -> t

(* [None] indicates termination *)
val transition : t -> RL.S.t -> RL.A.t -> RL.S.t option
