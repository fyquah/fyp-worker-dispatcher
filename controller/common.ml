[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

module Config = Protocol.Config
module Info_rpc = Protocol.Info_rpc
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc
module Relpath = Protocol.Relpath
module Execution_stats = Protocol.Execution_stats
module Results = Protocol.Results
module Work_unit_id = Results.Work_unit_id

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

let shell ?(env = []) ?(echo = false) ?(verbose = false) ~dir:working_dir
    prog args =
  Monitor.try_with_or_error (fun () ->
      let env = `Extend env in
      Async_shell.run ~echo ~verbose ~working_dir ~env prog args)
;;

let rec is_prefix
    ~(prefix : Call_site.t list) (test : Call_site.t list) =
  match prefix, test with
  | [], [] -> false
  | [], _ -> true
  | _, [] -> false
  | prefix_hd :: prefix_tl, hd :: tl ->
    Call_site.equal prefix_hd hd &&
    is_prefix ~prefix:prefix_tl tl

let filter_decisions (decisions : Data_collector.t list) =
  List.filter decisions ~f:(fun test ->
    not (
      List.exists decisions ~f:(fun decision ->
        if phys_equal test decision
        then false
        else (
          not decision.decision &&
          is_prefix ~prefix:(List.rev decision.call_stack) (List.rev test.call_stack)
        )
      )
    )
  )

let lift_deferred m = Deferred.(m >>| fun x -> Core.Or_error.return x)

let geometric_mean l =
  Float.exp
  List.fold l ~init:1.0 ~f:(fun a b -> a *. b) in
;;
