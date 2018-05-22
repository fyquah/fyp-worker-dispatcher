module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

open Import

let keys a =
  StringMap.bindings a |> List.map fst
;;

module Feature_list = struct
  type 'a t = 'a StringMap.t

  let empty = StringMap.empty

  let concat list_a list_b =
    StringMap.merge (fun key left right ->
        match left, right with
        | None, None -> None
        | Some a, None
        | None, Some a -> Some a
        | Some _, Some _ -> assert false)
      list_a list_b
  ;;

  let of_list list =
    List.fold_left (fun m (k, v) -> StringMap.add k v m)
      StringMap.empty list
  ;;

  let to_list a = 
    StringMap.bindings a
  ;;

  let data a = StringMap.bindings a |> List.map snd
end

type normaliser =
  { mean : float Feature_list.t;
    std  : float Feature_list.t;
  }

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
      (keys int_features);
      (keys numeric_features);
      (keys bool_features);
    ]
  ;;

  let find_exn { int_features; bool_features; numeric_features; } name = 
    match StringMap.find_opt name int_features with
    | Some x -> `Int x
    | None   ->
      match StringMap.find_opt name numeric_features with
      | Some x -> `Numeric x
      | None -> 
       `Bool (StringMap.find name bool_features)
  ;;

  let find_numeric_exn t name =
    match find_exn t name with
    | `Numeric x -> x
    | `Int x -> float_of_int x
    | `Bool x -> (if x then 1.0 else 0.0)
  ;;

  let verify_fields fields =
    match fields with
    | [] -> true
    | fields ->
      let reference = names (List.hd fields) |> StringSet.of_list in
      List.for_all (fun features ->
          StringSet.equal (StringSet.of_list (names features)) reference)
        fields
  ;;

  let to_array t =
    names t
    |> List.map (find_numeric_exn t)
    |> Array.of_list
  ;;

  let fold_numeric_features features_list ~f =
    assert (verify_fields features_list);
    let names = keys ((List.hd features_list).numeric_features) in
    List.fold_left (fun unchanged { numeric_features; _ } ->
        List.fold_left (fun unchanged key ->
            let name = key in
            let value = StringMap.find key numeric_features in
            StringMap.update name (fun candidate ->
                Some (f ~name:key ~acc:candidate value))
              unchanged)
          unchanged names)
      Feature_list.empty features_list
  ;;

  let create_normaliser_function { mean; std; } =
    `Staged (
      fun ({ numeric_features; int_features; bool_features } : [`raw] t) ->
        let numeric_features =
          StringMap.mapi (fun key data ->
              let mean = StringMap.find key mean in
              let std  = StringMap.find key std in

              if 0.0 = std then
                (data -. mean)
              else
                (data -. mean) /. std)
            numeric_features
        in
        ({ numeric_features; int_features; bool_features; } : [`normalised] t)
    )
  ;;

  let create_normaliser features_list =
    let num_examples = List.length features_list |> float_of_int in
    let mean =
      fold_numeric_features features_list ~f:(fun ~name:_ ~acc value ->
          match acc with
          | None -> value
          | Some x -> value +. x)
      |> StringMap.map (fun a -> a /. num_examples)
    in
    let std =
      fold_numeric_features features_list ~f:(fun ~name ~acc value ->
          let mean = StringMap.find name mean in
          let value = (value -. mean) ** 2.0 in
          match acc with
          | None -> value
          | Some x -> value +. x)
      |> StringMap.map (fun a -> a /. num_examples)
    in
    { mean; std; }
  ;;
end

let query_trace (data : Inlining_query.query) =
  List.map (fun (_, item) ->
      match item with
      | Data_collector.Trace_item.Enter_decl { declared; _ } ->
        Feature_extractor.Decl declared.closure_origin
      | Data_collector.Trace_item.At_call_site acs ->
        Feature_extractor.Apply acs.apply_id)
    data.env.inlining_stack
;;
