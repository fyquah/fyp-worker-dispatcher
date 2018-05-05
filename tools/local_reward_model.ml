open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils

let wait sec = Clock.after (Time.Span.of_sec sec)

module Epoch_snapshot = struct
  type entry = {
    accuracy : float;
    loss     : float;
  }
  [@@deriving sexp]

  type t = {
    epoch      : int;
    training   : entry option;
    validation : entry option;
    test       : entry option;
  }
  [@@deriving sexp]
end

let () =
  Command.group ~summary:"Local reward model" [
    ("familiarity-model", Familiarity_model.command)
  ]
  |> Command.run
;;

(*
let reward_traces =
  List.map raw_rewards ~f:(fun (trace, _, _) -> trace)
in
let feature_traces =
  List.map features ~f:(fun feature ->
      Absolute_path.compress (Absolute_path.of_trace feature.trace))
in
let print_traces buffer traces =
  List.map ~f:Absolute_path.sexp_of_t traces
  |> List.sort ~compare:Sexp.compare
  |> List.iter ~f:(fun trace ->
      bprintf buffer "%s\n" (Sexp.to_string_hum trace);
      bprintf buffer  ">>>>>>>>>>>>>>>>>>\n";
    );
  ;;
let _unused_debugging () =
  let%bind () =
    let buffer = Buffer.create 10 in
    print_traces buffer
      (List.map ~f:(fun a -> Absolute_path.of_trace (fst a).trace) examples);
    Writer.save "trace_from_examples.txt" ~contents:(Buffer.contents buffer)
  in
  let%bind () =
    let buffer = Buffer.create 10 in
    print_traces buffer reward_traces;
    Writer.save"trace_from_rewards.txt" ~contents:(Buffer.contents buffer)
  in
  let%bind () =
    let buffer = Buffer.create 10 in
    print_traces buffer feature_traces;
    Writer.save "trace_from_features.txt" ~contents:(Buffer.contents buffer)
  in
  Deferred.unit
in
*)
