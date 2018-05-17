open Core
open Async
open Fyp_compiler_lib
open Protocol.Shadow_fyp_compiler_lib
       
open Common


let process (query : Inlining_query.query) =
  let calc_t_matces expr ~f =
    let ctr = ref 0 in
    Flambda.iter_general ~toplevel:true
      (fun (t : Flambda.t) ->
         if f t then
           ctr := !ctr + 1)
      (fun (_named : Flambda.named) -> ())
      (Flambda.Is_expr expr);
    !ctr
  in
  let calc_named_matches expr ~f =
    let ctr = ref 0 in
    Flambda.iter_general ~toplevel:true
      (fun (_t : Flambda.t) -> ())
      (fun (named : Flambda.named) ->
         if f named then
           ctr := !ctr + 1)
      (Flambda.Is_expr expr);
    !ctr
  in
  let env = query.env in
  let calc_matching_approximations_from_env ~prefix (expr : Flambda.t) =
    let num_symbol_approximations =
      calc_named_matches expr ~f:(function
          | Flambda.Symbol s -> Symbol.Map.mem s env.approx_sym
          | _ -> false)
    in
    let num_mutable_approximations =
      calc_named_matches expr ~f:(function
          | Flambda.Read_mutable mvar ->
            Mutable_variable.Map.mem mvar env.approx_mutable
          | _ -> false)
    in
    let num_var_approximations =
      calc_t_matces expr ~f:(function
          | Flambda.Var var -> Variable.Map.mem var env.approx
          | _ -> false)
    in
    let num_closure_movements_matches =
      calc_named_matches expr ~f:(fun expr ->
          match expr with
          | Flambda.Move_within_set_of_closures msvc ->
            Projection.Map.mem
              (Projection.Move_within_set_of_closures msvc)
              env.projections
          | _ -> false)
    in
    let num_project_closure_matches =
      calc_named_matches expr ~f:(fun expr ->
          match expr with
          | Flambda.Project_closure pc ->
            Projection.Map.mem
              (Projection.Project_closure pc)
              env.projections
          | _ -> false)
    in
    let num_project_var_matches =
      calc_named_matches expr ~f:(fun expr ->
          match expr with
          | Flambda.Project_var pv ->
            Projection.Map.mem
              (Projection.Project_var pv)
              env.projections
          | _ -> false)
    in
    let p = prefix in
    let int_features =
      Feature_list.of_list [
        (p ^ "_symbol_approx",              num_symbol_approximations);
        (p ^ "_mutable_approx",             num_mutable_approximations);
        (p ^ "_var_approx",                 num_var_approximations);
        (p ^ "_move_within_closure_approx", num_closure_movements_matches);
        (p ^ "_project_closure_approx",     num_project_closure_matches);
        (p ^ "_project_var_approx",         num_project_var_matches);
      ]
    in
    let bool_features = Feature_list.empty in
    { Features. int_features; bool_features }
  in
  let callee_features ~prefix expr =
    let size = Inlining_cost.lambda_size expr in
    let num_const_int =
      calc_named_matches expr ~f:(fun (named : Flambda.named) ->
          match named with
          | Flambda.Const (Flambda.Char _)
          | Flambda.Const (Flambda.Int _) -> true
          | _ -> false)
    in
    let num_const_pointer =
      calc_named_matches expr ~f:(fun (named : Flambda.named) ->
          match named with
          | Flambda.Const (Flambda.Const_pointer _) -> true
          | _ -> false)
    in
    let p = prefix in
    let int_features =
      Feature_list.of_list [
        (p ^ "_const_int", num_const_int);
        (p ^ "_const_pointer", num_const_pointer);
        (p ^ "_size", size);
      ]
    in
    let bool_features = Feature_list.empty in
    { Features. int_features; bool_features; }
  in
  let parameter_features =
    let num_invariant_params =
      let var = Closure_id.unwrap query.closure_id_being_applied in
      Variable.Map.find_opt var query.value_set_of_closures.invariant_params
      |> Option.map ~f:Variable.Set.cardinal
      |> Option.value ~default:0
    in
    let num_specialised_args =  (* TODO: Is this even valid? *)
      let params = query.function_decl.params in
      List.filter params ~f:(fun p ->
          let var = Parameter.var p in
          Variable.Map.mem var query.value_set_of_closures.specialised_args)
      |> List.length
    in
    let num_params = List.length query.function_decl.params in

    let int_features =
      Feature_list.of_list [
        ("num_invariant_params", num_invariant_params);
        ("num_specialised_args", num_specialised_args);
        ("num_params", num_params);
      ]
    in
    let bool_features = Feature_list.empty in
    { Features. int_features; bool_features; } 
  in
  let env_features =
    let int_features =
      Feature_list.of_list [
        ("inlining_level", env.inlining_level);
        ("closure_depth", env.closure_depth);
        ("inside_branch", env.inside_branch);
      ]
    in
    let bool_features = Feature_list.empty in
    { Features. int_features; bool_features; }
  in
  let open Features in
  calc_matching_approximations_from_env ~prefix:"original" query.original
  @ calc_matching_approximations_from_env ~prefix:"inlined" query.inlined_result.body
  @ env_features
  @ parameter_features
  @ callee_features ~prefix:"original" query.original
  @ callee_features ~prefix:"inlined" query.inlined_result.body
;;


let command =
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
          Io_helper.load_queries ~allow_repeat:() ~filelist
          |> Pipe.map ~f:process
          |> Pipe.to_list
        in
        let names = Features.names (List.hd_exn all_features) in
        let header = String.concat ~sep:"," names in
        let rows =
          List.map all_features ~f:(fun features ->
              List.map names ~f:(fun name ->
                match Features.find_exn features name with
                  | `Int x -> x
                  | `Bool x -> (if x then 1 else 0))
              |> List.map ~f:Int.to_string
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
