[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Offset : sig
  type t

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t

  val base   : t
  val inc    : t -> t
  val to_int : t -> int
  val equal  : t -> t -> bool
end

type at_call_site =
  { source    : Closure_id.t option;
    offset    : Offset.t;
    applied   : Closure_id.t;
  }

type enter_decl =
  { source     : Closure_id.t option;
    closure    : Closure_id.t;
  }

type t =
  | Enter_decl of enter_decl
  | At_call_site of at_call_site

val equal : t -> t -> bool

val enter_decl : source: Closure_id.t option -> closure: Closure_id.t -> t

val create_top_level : Closure_id.t -> Offset.t -> t

val create : source: Closure_id.t -> applied: Closure_id.t -> Offset.t -> t

val sexp_of_t : t -> Sexp.t

val t_of_sexp : Sexp.t -> t

val pprint : Format.formatter -> t -> unit
