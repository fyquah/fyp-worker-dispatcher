open Core
open Async

module Inlining_tree = Protocol.Inlining_tree
module Data_collector = Protocol.Shadow_fyp_compiler_lib.Data_collector

module T1 = struct
  let name = "compiler-call-site-offset"

  let datadir = Testlib.repo_root ^/ "test/testdata/call-site-offset"

  let load_decision_tree () =
    let%bind decisions =
      Reader.load_sexp_exn (datadir ^/ "main.0.data_collector.sexp")
        [%of_sexp: Data_collector.t list]
    in
    let decision_tree = Inlining_tree.build decisions in
    return decision_tree
  ;;

  let compile_with_decisions root =
    let working_dir = datadir in
    let decisions = Inlining_tree.Top_level.to_override_rules root in
    let%bind () =
      Writer.save_sexp (datadir ^/ "overrides.sexp")
        ([%sexp_of: Data_collector.t list] decisions)
    in
    Async_shell.run ~expect:[ 0 ] ~working_dir
      "bash" [ "-c"; "eval `opam config env` && make all" ]
  ;;

  let run () =
    let%bind () = compile_with_decisions [] in
    let%bind tree = load_decision_tree () in
    printf "tree 1:\n%s" (Format.asprintf "%a" Inlining_tree.Top_level.pp tree);
    let modified_tree =
      let leaves = Inlining_tree.Top_level.count_leaves tree in
      Inlining_tree.Top_level.flip_nth_leaf tree (leaves - 1)
    in
    let%bind () = compile_with_decisions modified_tree in
    let%bind tree = load_decision_tree () in
    printf "tree 2:\n%s" (Format.asprintf "%a" Inlining_tree.Top_level.pp tree);
    Deferred.unit
end

let () =
  Testlib.register (module T1);
