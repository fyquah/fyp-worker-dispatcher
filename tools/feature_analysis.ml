open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils

let () =
  let readme () =
    "Models that provide some form of feature analysis for the purpose of \
     unsupervised learning."
  in
  Command.group ~summary:"Models that provide analyse features" ~readme [
    ("manual-features-v1", Manual_features_v1.command);
  ]
  |> Command.run
;;
