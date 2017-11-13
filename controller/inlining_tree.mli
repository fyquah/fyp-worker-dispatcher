[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Common

exception Build_error

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

  val flip_nth_leaf : t -> int -> t

  val flip_several_leaves : t -> int list -> t
end

val shallow_sexp_of_t : t -> Sexp.t

val fuzzy_equal : t -> t -> bool

val add : Top_level.t -> Data_collector.t -> Top_level.t

val build : Data_collector.t list -> Top_level.t
