(*
 * [Data_collector] is a terrible name as this module doesn't actually
 * perform any data collection.
 *
 * It provides a type to denote every inlining decision, that is it
 *   - allows the compiler to store all the decisions it had made
 *   - allows the user to specify a list of inlining decisions override that
 *     the user wants to make
 *
 * V0 - Started 16 October 2017, records by Closure_id and a custom offset
 * V1 - Started 2 February 2018, records by [Closure_origin] and [Apply_id]
 *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module V0 : sig

  module Query : sig
    type t =
      { call_stack : Call_site.t list;
        applied: Closure_id.t;
      }
  end

  type t =
    { call_stack : Call_site.t list;
      applied : Closure_id.t;
      decision : bool;
    }
  
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  
  val inlining_decisions : t list ref
  
  val save : output_prefix: string -> unit
  
  val load_from_channel : in_channel -> t list
  
  val pprint_list : Format.formatter -> t list -> unit
  
  val equal : t -> t -> bool
  
  val find_decision
     : t list
    -> call_stack: Call_site.t list
    -> applied: Closure_id.t
    -> bool option
end

module V1 : sig
  include Data_collector_intf.S
end


(* TODO(fyq14): Surely there is a more concise way to write these type signature? *)
module Function_metadata = V1.Function_metadata
module Trace_item = V1.Trace_item
module Action = V1.Action
module Decision = V1.Decision
module Overrides = V1.Overrides

module Simple_overrides : sig
  type t

  val load_from_channel : in_channel -> t

  val of_v1_overrides : Overrides.t -> t

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t
end


(* Strictly used only by the compiler to make overrides, so no need sexp
 * functions here.
 *)
module Multiversion_overrides : sig
  type t

  type query = V0.Query.t * V1.Overrides.query

  val find_decision : t -> query -> V1.Action.t option

  val load_from_clflags : unit -> t

  val don't : t
end
