open Common

type node =
  { inline: Closure_id.t;
    no_inline: Closure_id.t;
  }

type t = private
  { node_map: node Closure_id.Map.t;
    root: Closure_id.t;
  }

val transition : t -> Closure_id.t -> RL.A.t -> Closure_id.t option
