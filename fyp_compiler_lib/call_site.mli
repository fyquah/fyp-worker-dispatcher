[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type offset
type t

val equal : t -> t -> bool

val base_offset : offset

val inc : offset -> offset

val enter_decl : Closure_id.t -> t

val create_top_level : offset -> t

val create : Closure_id.t -> offset -> t

val to_sexp : t -> Sexp.t

val of_sexp : Sexp.t -> t

val pprint : Format.formatter -> t -> unit
