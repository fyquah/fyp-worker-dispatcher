open Core
open Common
open Data_collector.V1

(* Builds a control flow graph of inlining decisions in the entire
 * source tree.
 *)

module RL = Rl

(* TODO(fyq14): Consider descending into declarations and make
 *              a separate call graph for each one of them?
 *
 *              I believe this might make convergence faster as
 *              it gives a more greedy solution.
 *)
module Function_call : sig
  type t =
    { (* [inlining_trace] here Follows the same convention as those in
       * [Data_collector.t] (aka the override type), that is the first
       * element is the T.O.S. The earliest function call is the last in
       * call_stack
       *)
      inlining_trace: (Apply_id.t * Function_metadata.t) list;
      apply_id:       Apply_id.t;
      applied:        Function_metadata.t;
    }
  [@@deriving sexp, compare]

  include Comparable.S with type t := t

  val t_of_decision : Data_collector.V1.Decision.t -> t option
end

type node =
  { inline:    RL.S.t;
    no_inline: RL.S.t;
  }

type t = private
  { transitions:    node RL.S.Map.t;
    root:           RL.S.t;
    function_calls: Function_call.t RL.S.Map.t;
    reverse_map:    RL.S.t Function_call.Map.t;
  }
[@@deriving sexp_of]

val t_of_inlining_tree : Inlining_tree.V1.Top_level.t -> t

val overrides_of_pending_trajectory
   : t
  -> RL.Pending_trajectory.t
  -> Data_collector.V1.Overrides.t

(* [None] indicates termination *)
val transition : t -> RL.S.t -> RL.A.t -> RL.S.t

val pprint : ?with_legend:unit -> t -> string
