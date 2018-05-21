open Core
open Common

type t =
  { specification_file : string;
    epochs             : int;
    hyperparams_file   : string;
    feature_version    : feature_versions;
    dump_graph         : string option;
    checkpoint         : string option;
  }

let training =
  let open Command.Let_syntax in
  [%map_open
    let specification_file =
      flag "-spec" (required file) ~doc:"FILE specification file"
    and epochs =
      flag "-epochs" (required int) ~doc:"INT epochs"
    and hyperparams_file =
      flag "-hyperparams" (required file) ~doc:"FILE hyperparams file"
    and feature_version =
      flag "-feature-version" (required string) ~doc:"STRING feature version"
    and dump_graph =
      flag "-dump-graph" (optional string) ~doc:"STRING dump graph"
    and checkpoint =
      flag "-checkpoint" (optional string) ~doc:"STRING dump weights"
    in
    let feature_version = parse_version feature_version |> Option.value_exn in
    { specification_file; epochs; hyperparams_file; feature_version;
      dump_graph; checkpoint;
    }
  ]
