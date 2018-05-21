open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils
open Common


let command_manual_features_v1 =
  let open Command.Let_syntax in
  Command.async ~summary:"Manual hand features v1"
    [%map_open
      let filelist =
        flag "-filelist" (required string) ~doc:"FILE data source"
      and output =
        flag "-output" (required string) ~doc:"FILE output"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind filelist = Reader.file_lines filelist in
        let%bind all_features =
          Io_helper.load_queries ~allow_repeat:`Only_across_files ~filelist
          |> Pipe.map ~f:Manual_features_v1.process
          |> Pipe.to_list
        in
        let names = Features.names (List.hd_exn all_features) in
        let header = String.concat ~sep:"," names in
        let rows =
          List.map all_features ~f:(fun features ->
              List.map names ~f:(fun name ->
                match Features.find_exn features name with
                  | `Numeric x -> x
                  | `Int x -> Float.of_int x
                  | `Bool x -> (if x then 1.0 else 0.0))
              |> List.map ~f:Float.to_string
              |> String.concat ~sep:",")
        in
        let%bind () =
          Writer.with_file output ~f:(fun wrt ->
              Writer.write_line wrt header;
              List.iter rows ~f:(Writer.write_line wrt);
              Deferred.unit)
        in
        return ()
    ]
;;


let () =
  let readme () =
    "Models that provide some form of feature analysis for the purpose of \
     unsupervised learning."
  in
  Command.group ~summary:"Models that provide analyse features" ~readme [
    ("manual-features-v1", command_manual_features_v1);
  ]
  |> Command.run
;;
