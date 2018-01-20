[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

include Protocol.Shadow_fyp_compiler_lib

module Inlining_tree = Protocol.Inlining_tree
module Config = Protocol.Config
module Info_rpc = Protocol.Info_rpc
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc
module Relpath = Protocol.Relpath
module Execution_stats = Protocol.Execution_stats
module Results = Protocol.Results
module Work_unit_id = Results.Work_unit_id

let shell_echo_default = ref false
let shell_verbose_default = ref false

let shell ?(env = []) ?(echo) ?(verbose) ~dir:working_dir
    prog args =
  let echo = Option.value ~default:!shell_echo_default echo in
  let verbose = Option.value ~default:!shell_verbose_default verbose in
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
  let power = 1.0 /. float_of_int (List.length l) in
  let base = List.fold l ~init:1.0 ~f:(fun a b -> a *. b) in
  base ** power
;;

let gmean_exec_time t =
  t.Execution_stats.raw_execution_time
  |> List.map ~f:Time.Span.to_sec
  |> geometric_mean
  |> Time.Span.of_sec
;;
