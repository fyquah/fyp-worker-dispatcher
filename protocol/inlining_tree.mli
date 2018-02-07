[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Shadow_fyp_compiler_lib

exception Build_error

module V0 : sig
  type t =
    | Declaration of declaration
    | Apply_inlined_function of inlined_function
    | Apply_non_inlined_function of non_inlined_function
  and declaration =
    { closure   : Closure_id.t;
      children  : t list;
    }
  and non_inlined_function =
    { applied   : Closure_id.t;
      offset    : Call_site_offset.t;
    }
  and inlined_function =
    { applied   : Closure_id.t;
      offset    : Call_site_offset.t;
      children  : t list;
    }
  [@@deriving sexp, compare]
  
  module Top_level : sig
    type nonrec t = t list [@@deriving sexp]
  
    val count_leaves : t -> int
  
    val backtrack_nth_leaf : t -> int -> t option
  
    val flip_nth_leaf : t -> int -> t
  
    val flip_several_leaves : t -> int list -> t
  
    val to_override_rules : t -> Data_collector.V0.t list
  
    val pp : Format.formatter -> t -> unit
  end
  
  module Diff : sig
    type nonrec t =
      { common_ancestor : t list; (* Arbitary choice between the left and right *)
        left            : [ `Left of t ]  list;
        right           : [ `Right of t ] list;
      }
  end
  
  val diff : left: Top_level.t -> right: Top_level.t -> Diff.t list
  
  val shallow_sexp_of_t : t -> Sexp.t
  
  val fuzzy_equal : t -> t -> bool
  
  val add : Top_level.t -> Data_collector.V0.t -> Top_level.t
  
  val build : Data_collector.V0.t list -> Top_level.t
end
