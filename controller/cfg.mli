open Common

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
    { top_level_offset: Call_site.Offset.t;
      call_stack: (Closure_id.t * Call_site.Offset.t) list;
      applied:    Closure_id.t;
    }
  [@@deriving sexp]
end

type node =
  { inline: RL.S.t option;
    no_inline: RL.S.t option;
  }

type t = private
  { transitions: node RL.S.Map.t;
    root:           RL.S.t;
    function_calls:  Function_call.t RL.S.Map.t
  }

val t_of_inlining_tree : Inlining_tree.Top_level.t -> t

(* [None] indicates termination *)
val transition : t -> RL.S.t -> RL.A.t -> RL.S.t option

val pprint : ?with_legend:unit -> t -> string
