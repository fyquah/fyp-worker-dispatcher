open Core
open Async
open Protocol.Shadow_fyp_compiler_lib
open Mat_utils

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
      in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind specification =
          Reader.load_sexp_exn specification_file
            Specification_file.t_of_sexp
        in
        let%map examples =
          load_from_specification specification
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

let command =
  Command.group ~summary:"Binary to generate plot data"
  [("dump-rewards", command_dump_rewards);
  ]
;;
