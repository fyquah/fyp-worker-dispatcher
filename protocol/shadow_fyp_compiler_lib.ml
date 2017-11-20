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
  include Fyp_compiler_lib.Data_collector
  include Make_core_sexp(Fyp_compiler_lib.Data_collector)
end

module Closure_id = struct
  include Fyp_compiler_lib.Closure_id
  include Make_core_sexp(Fyp_compiler_lib.Closure_id)
end

module Call_site = struct
  include Fyp_compiler_lib.Call_site
  include Make_core_sexp(Fyp_compiler_lib.Call_site)

end

module Call_site_offset = struct
  include Call_site.Offset
  include Make_core_sexp(Call_site.Offset)

  let compare a b = Int.compare (to_int a) (to_int b)
end
