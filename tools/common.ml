open Core
open Protocol.Shadow_fyp_compiler_lib

type feature_versions = [ `V0 | `V1 ]

let parse_version v =
  match v with
  | "V0" -> Some `V0
  | "V1" -> Some `V1
  | _    -> None
;;

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
  type 'a t =
    { int_features     : int   Feature_list.t;
      numeric_features : float Feature_list.t;
      bool_features    : bool  Feature_list.t;
    }

  let concat a b =
    { int_features     =  Feature_list.concat a.int_features     b.int_features;
      numeric_features =  Feature_list.concat a.numeric_features b.numeric_features;
      bool_features    =  Feature_list.concat a.bool_features    b.bool_features
    }

  let (@) a b = concat a b

  let names { int_features; bool_features; numeric_features; } =
    List.concat [
      (String.Map.keys int_features);
      (String.Map.keys numeric_features);
      (String.Map.keys bool_features);
    ]
  ;;

  let find_exn { int_features; bool_features; numeric_features; } name = 
    match String.Map.find int_features name with
    | Some x -> `Int x
    | None   ->
      match String.Map.find numeric_features name with
      | Some x -> `Numeric x
      | None -> 
       `Bool (String.Map.find_exn bool_features name)
  ;;

  let find_numeric_exn t name =
    match find_exn t name with
    | `Numeric x -> x
    | `Int x -> Float.of_int x
    | `Bool x -> (if x then 1.0 else 0.0)
  ;;

  let verify_fields l =
    match l with
    | [] -> true
    | l ->
      let reference = names (List.hd_exn l) |> String.Set.of_list in
      List.for_all l ~f:(fun features ->
          String.Set.equal (String.Set.of_list (names features)) reference)
  ;;

  let to_array t =
    let names = names t in
    Array.of_list_map names ~f:(fun name ->
        find_numeric_exn t name)
  ;;

  let fold_numeric_features features_list ~f =
    assert (verify_fields features_list);
    let names =
      String.Map.keys ((List.hd_exn features_list).numeric_features)
    in
    List.fold features_list ~init:Feature_list.empty
      ~f:(fun unchanged { numeric_features; _ } ->
          List.fold names ~init:unchanged ~f:(fun unchanged key ->
              let name = key in
              let value = String.Map.find_exn numeric_features key in
              String.Map.update unchanged name ~f:(fun candidate ->
                  f ~name:key ~acc:candidate value)))
  ;;

  let create_normaliser features_list =
    let num_examples = List.length features_list |> Float.of_int in
    let means =
      fold_numeric_features features_list ~f:(fun ~name:_ ~acc value ->
          match acc with
          | None -> value
          | Some x -> value +. x)
      |> String.Map.map ~f:(fun a -> a /. num_examples)
    in
    let std =
      fold_numeric_features features_list ~f:(fun ~name ~acc value ->
          let mean = String.Map.find_exn means name in
          let value = Float.((value -. mean) ** 2.0) in
          match acc with
          | None -> value
          | Some x -> value +. x)
      |> String.Map.map ~f:(fun a -> a /. num_examples)
    in
    Staged.stage (
      fun ({ numeric_features; int_features; bool_features } : [`raw] t) ->
        let numeric_features =
          String.Map.mapi numeric_features ~f:(fun ~key ~data ->
              let mean = String.Map.find_exn means key in
              let std  = String.Map.find_exn std key in

              if Float.equal 0.0 std then
                (data -. mean)
              else
                (data -. mean) /. std)
        in
        ({ numeric_features; int_features; bool_features; } : [`normalised] t)
    )
  ;;

  let create_normaliser_to_owl_vec features_list =
    let f = Staged.unstage (create_normaliser features_list) in
    Staged.stage (
      fun raw_features ->
        f raw_features
        |> to_array
        |> (fun a -> Owl.Mat.of_array a 1 (Array.length a))
    )
  ;;
end

let query_trace (data : Inlining_query.query) =
  List.map data.env.inlining_stack ~f:(fun (_, item) ->
      match item with
      | Data_collector.Trace_item.Enter_decl { declared; _ } ->
        Feature_extractor.Decl declared.closure_origin
      | Data_collector.Trace_item.At_call_site acs ->
        Feature_extractor.Apply acs.apply_id)
;;
