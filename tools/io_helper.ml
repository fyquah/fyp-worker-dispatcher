open Core
open Async
open Protocol.Shadow_fyp_compiler_lib


module type Container_intf = sig
  type 'a t

  val empty : 'a t

  val add : 'a t -> key: Protocol.Absolute_path.t -> data: 'a -> 'a t

  val data : 'a t -> 'a list
end

module Container_list : Container_intf = struct
  type 'a t = 'a list

  let empty = []

  let add l ~key:_ ~data:x = x :: l

  let data l = l
end

module Container_map : Container_intf = struct
  type 'a t = 'a Protocol.Absolute_path.Map.t

  let empty = Protocol.Absolute_path.Map.empty

  let add map ~key ~data =
    match Protocol.Absolute_path.Map.add map ~key ~data with
    | `Duplicate -> map
    | `Ok map -> map
  ;;

  let data map = Protocol.Absolute_path.Map.data map
end

let load_queries ?(allow_repeat = `No) ~filelist =
  let rdr, wrt = Pipe.create () in
  let seen_so_far = ref Protocol.Absolute_path.Set.empty in
  let module Container = Container_map in
  don't_wait_for (
    Deferred.List.iter filelist ~how:`Sequential ~f:(fun filename ->
      Reader.with_file filename (fun rdr ->
        let%bind (value : Inlining_query.query list Async.Reader.Read_result.t) =
          Async.Reader.read_marshal rdr
        in
        let value =
          match value with
          | `Eof -> []
          | `Ok value -> value
        in
        let value =
          List.fold value ~init:Container.empty ~f:(fun container data ->
              let trace =
                List.map data.env.inlining_stack ~f:(fun (_, item) ->
                    match item with
                    | Data_collector.Trace_item.Enter_decl { declared; _ } ->
                      Feature_extractor.Decl declared.closure_origin
                    | Data_collector.Trace_item.At_call_site acs ->
                      Feature_extractor.Apply acs.apply_id)
                |> Protocol.Absolute_path.of_trace
              in
              let key = trace in
              let next =
                match allow_repeat with
                | `No ->
                   if Protocol.Absolute_path.Set.mem !seen_so_far key then
                     container
                   else
                     Container.add container ~key ~data
                | `Only_across_files 
                | `Yes ->
                   Container.add container ~key ~data
              in
              seen_so_far := Protocol.Absolute_path.Set.add !seen_so_far key;
              next)
        in
        Deferred.List.iter (Container.data value) ~how:`Sequential ~f:(fun query ->
          Pipe.write wrt query)))
    >>| fun () -> Pipe.close wrt
  );
  rdr
;;
