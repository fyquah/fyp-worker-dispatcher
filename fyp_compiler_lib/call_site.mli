[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type offset

type at_call_site =
  { source    : Closure_id.t option;
    offset    : offset;
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

val base_offset : offset

val inc : offset -> offset

val enter_decl : source: Closure_id.t option -> closure: Closure_id.t -> t

val create_top_level : Closure_id.t -> offset -> t

val create : source: Closure_id.t -> applied: Closure_id.t -> offset -> t

val to_sexp : t -> Sexp.t

val of_sexp : Sexp.t -> t

val pprint : Format.formatter -> t -> unit
