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

let load_queries ?(allow_repeat = ()) ~filelist =
  let rdr, wrt = Pipe.create () in
  let module Container = Container_map in
  don't_wait_for (
    Deferred.List.iter filelist ~how:`Sequential ~f:(fun filename ->
      let%bind rdr = Reader.open_file filename in
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
            Container.add container ~key ~data)
      in
      Deferred.List.iter (Container.data value) ~f:(fun query ->
        Pipe.write wrt query))
  );
  rdr
;;
