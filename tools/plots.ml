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
      let filename = anon ("filename" %: string) in
      fun () ->
        let open Deferred.Let_syntax in
        let%bind examples =
          Reader.load_sexp_exn filename [%of_sexp: Raw_data.Reward.t list]
          >>| fun rewards ->
          List.map rewards ~f:(fun reward ->
            { reward with path = Absolute_path.expand reward.path })
        in
        List.map examples ~f:(fun (reward : Raw_data.Reward.t) ->
          let inline = reward.inline_reward in
          let no_inline = reward.no_inline_reward in
          { Reward_dump. inline; no_inline; })
        |> [%sexp_of: Reward_dump.t list]
        |> Sexp.to_string
        |> printf "%s";
        Deferred.unit
    ]
;;

let command_describe_rewards =
  let open Command.Let_syntax in
  Command.async ~summary:"describe the path in features"
  [%map_open
    let filename = anon ("filename" %: string) in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind examples =
        Reader.load_sexp_exn filename [%of_sexp: Raw_data.Reward.t list]
        >>| fun rewards ->
        List.map rewards ~f:(fun reward ->
          { reward with path = Absolute_path.expand reward.path })
      in
      List.iter examples ~f:(fun (reward : Raw_data.Reward.t) ->
        let path = reward.path in
        let termination =
          match reward.no_inline_reward with
          | None -> "-INF"
          | Some term ->
            if term >= 0.0 then
              sprintf "-%4f" term
            else
              sprintf " %4f" (abs_float term)
        in
        let long_term =
          match reward.inline_reward with
          | None -> " -INF "
          | Some x ->
            if x.long_term >= 0.0 then
              sprintf "-%.4f" x.long_term
            else
              sprintf " %.4f" (abs_float x.long_term)
        in
        printf "- (%s | %s)\t" long_term termination;
        List.iter path ~f:(function
          | Decl co ->
            printf "%s" (Format.asprintf "/Decl(%a)" Closure_origin.print co)
          | Apply app ->
            printf "%s" (sprintf "/Apply(%s)" (Apply_id.Node_id.to_string (List.last_exn app))));
        printf "\n";
      );
      Deferred.unit
  ]
;;

let command_describe_features =
  let open Command.Let_syntax in
  Command.async ~summary:"describe the path in features"
  [%map_open
    let filename =
      flag "-filename" (required file) ~doc:"FILE specification file"
    and feature_version =
      flag "-feature-version" (required string) ~doc:"Feature version"
    in
    fun () ->
      let open Deferred.Let_syntax in
      let%bind examples =
        read_marshal_exn filename
        >>| List.filter ~f:(fun (query : Inlining_query.query) ->
            (query.env.round = 0))
        >>| List.map ~f:(fun (query : Inlining_query.query) ->
            (query_trace query, Manual_features_v1.process query))
      in
      let names =
        List.hd_exn examples |> snd |> Features.names
      in
      printf "Path ";
      List.iter names ~f:(fun name -> printf "%s " name);
      printf "\n";
      List.iter examples ~f:(fun (trace, (features : 'a Features.t)) ->
        List.iter (Absolute_path.(expand (of_trace trace))) ~f:(function
          | Decl co ->
            printf "%s" (Format.asprintf "/Decl(%a)" Closure_origin.print co)
          | Apply app ->
            printf "%s" (sprintf "/Apply(%s)" (Apply_id.Node_id.to_string (List.last_exn app))));
        printf " ";

        List.iter names ~f:(fun name ->
          match Features.find_exn features name with
          | `Int d     -> printf "%d " d
          | `Numeric f -> printf "%f " f
          | `Bool b    -> printf "%b " b);

        printf "\n";
      );
      printf "Number of examples = %d\n" (List.length examples);
      Deferred.unit
    ]
;;

let command_dump_data =
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
   ("describe-features", command_describe_features);
   ("describe-rewards", command_describe_rewards);
   ("dump-data", command_dump_data);
  ]
;;
