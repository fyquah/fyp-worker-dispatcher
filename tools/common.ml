open Core

module Feature_list = struct
  type 'a t = 'a String.Map.t

  let empty = String.Map.empty

  let concat a b =
    String.Map.merge a b ~f:(fun ~key value ->
        match value with
        | `Both (a, b) -> assert false
        | `Left a
        | `Right a -> Some a)
  ;;

  let of_list a = String.Map.of_alist_exn a
end


module Features = struct
  type t =
    { int_features  : int  Feature_list.t;
      bool_features : bool Feature_list.t;
    }

  let concat a b =
    { int_features =  Feature_list.concat a.int_features  b.int_features;
      bool_features = Feature_list.concat a.bool_features b.bool_features
    }

  let (@) a b = concat a b

  let names { int_features; bool_features; } =
    List.append
      (String.Map.keys int_features)
      (String.Map.keys bool_features)
  ;;

  let find_exn { int_features; bool_features } name = 
    match String.Map.find int_features name with
    | Some x -> `Int x
    | None   -> `Bool (String.Map.find_exn bool_features name)
  ;;
end
