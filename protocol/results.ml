open Core

let rec comp_sexp_of_core_sexp core_sexp =
  match core_sexp with
  | Core.Sexp.Atom s -> Fyp_compiler_lib.Sexp.Atom s
  | Core.Sexp.List l ->
    Fyp_compiler_lib.Sexp.List (List.map ~f:comp_sexp_of_core_sexp l)
;;

let rec core_sexp_of_comp_sexp comp_sexp =
  match comp_sexp with
  | Fyp_compiler_lib.Sexp.Atom s -> Core.Sexp.Atom s
  | Fyp_compiler_lib.Sexp.List l ->
    Core.Sexp.List (List.map ~f:core_sexp_of_comp_sexp l)
;;

module Make_core_sexp(M :
  sig type t

  val sexp_of_t : t -> Fyp_compiler_lib.Sexp.t
  val t_of_sexp : Fyp_compiler_lib.Sexp.t -> t
end) = struct
  let sexp_of_t t = core_sexp_of_comp_sexp (M.sexp_of_t t)
  let t_of_sexp sexp = M.t_of_sexp (comp_sexp_of_core_sexp sexp)
end

module Data_collector = struct
  include Fyp_compiler_lib.Data_collector.V0
  include Make_core_sexp(Fyp_compiler_lib.Data_collector.V0)
end

module Work_unit_id : sig
  type t [@@deriving sexp]
  include Hashable.S with type t := t
  include Stringable.S with type t := t
  include Comparable.S with type t := t

  val gen : unit -> t
end = struct
  module T = struct
    type t = int [@@deriving sexp, hash]

    let hash = Int.hash

    let compare = Int.compare

    let acc = ref 0

    let gen () =
      let ret = !acc in
      acc := ret + 1;
      ret

    let to_string = Int.to_string
    let of_string = Int.of_string
  end

  include T
  include Hashable.Make(T)
  include Comparable.Make(T)
end

type t =
  { id        : Work_unit_id.t;
    overrides : Data_collector.t list;
    decisions : Data_collector.t list;
    benchmark : Execution_stats.t;
    path_to_bin : string sexp_option;
  }
[@@deriving sexp]
