open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils
open Common

module Reward_dump = struct
  type t =
    { inline    : Raw_data.Reward.dual_reward option;
      no_inline : float option;
    }
  [@@deriving sexp]
end

let command_dump_rewards =
  let open Command.Let_syntax in
  Command.async ~summary:"something something"
    [%map_open
      let specification_file =
        flag "-spec" (required file) ~doc:"FILE specification file"
      and feature_version =
        flag "-feature-version" (required string) ~doc:"STRING feature version"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let version = Option.value_exn (parse_version feature_version) in
        let%bind specification =
          Reader.load_sexp_exn specification_file
            Specification_file.t_of_sexp
        in
        let%map examples =
          load_from_specification ~version specification
          >>| fun (a, b) -> a @ b
        in
        List.map examples ~f:(fun (_, (target : Raw_data.target)) ->
          let inline = fst target in
          let no_inline = snd target in
          { Reward_dump. inline; no_inline; })
        |> [%sexp_of: Reward_dump.t list]
        |> Sexp.to_string
        |> printf "%s"
    ]
;;

let command_dump_v1_data =
  let open Command.Let_syntax in
  Command.async ~summary:"v1 features"
    [%map_open
      let specification_file =
        flag "-spec" (required file) ~doc:"FILE specification file"
      and feature_version =
        flag "-feature-version" (required string) ~doc:"STRING feature version"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let version = Option.value_exn (parse_version feature_version) in
        let%bind specification =
          Reader.load_sexp_exn specification_file
            Specification_file.t_of_sexp
        in
        let%map examples =
          match specification with
          | Specification_file.Unsegmented entries -> 
            load_call_site_examples_raw ~version entries
          | _ -> assert false
        in
        List.map examples ~f:(fun (features, (target : Raw_data.target option)) ->
          let target =
            Option.map target ~f:(fun (inline, no_inline) ->
              { Reward_dump. inline; no_inline; })
          in
          (features, target))
        |> [%sexp_of: (Features.t * Reward_dump.t option) list]
        |> Sexp.to_string
        |> printf "%s"
    ]
;;

let command =
  Command.group ~summary:"Binary to generate plot data"
  [("dump-rewards", command_dump_rewards);
   ("dump-v1-data", command_dump_v1_data);
  ]
;;
