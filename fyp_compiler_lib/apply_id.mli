[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** [Variable.t] is the equivalent of a non-persistent [Ident.t] in
    the [Flambda] tree.  It wraps an [Ident.t] together with its source
    [compilation_unit].  As such, it is unique within a whole program,
    not just one compilation unit.

    Introducing a new type helps in tracing the source of identifiers
    when debugging the inliner.  It also avoids Ident renaming when
    importing cmx files.
*)

type label = [ `Plain_apply | `Over_application | `Stub ]

type stamp =
  | Plain_apply of int
  | Over_application of int
  | Stub

val sexp_of_stamp : stamp -> Sexp.t
val stamp_of_sexp : Sexp.t -> stamp

type t = private {
    compilation_unit : Compilation_unit.t;
    stamp            : stamp;
    parents          : t list option;
  }

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t

val equal : t -> t -> bool

val create : ?current_compilation_unit:Compilation_unit.t -> label -> t

val create_old : ?current_compilation_unit:Compilation_unit.t -> label -> t

val build_directly : Compilation_unit.t -> stamp -> t list -> t

val change_label : t -> label -> t

val in_compilation_unit : t -> Compilation_unit.t -> bool

val get_compilation_unit : t -> Compilation_unit.t

val print : Format.formatter -> t -> unit

val get_stamp : stamp -> int option

val compare_stamp : stamp -> stamp -> int

val equal_accounting_deprecation : t -> t -> bool

(* This is the identifier denoted in the unique path-labelling section of
 * the thesis
 *)
val get_inlining_path : t -> (Compilation_unit.t * stamp) list

val inline : caller: t -> inlined: t -> t

val to_identifier : t -> string
