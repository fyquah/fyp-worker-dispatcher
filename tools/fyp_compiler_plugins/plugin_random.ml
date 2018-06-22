let f (env : Inline_and_simplify_aux.Env.t) (query : Inlining_query.query lazy_t) =
  let limit = 30 in
  if Inline_and_simplify_aux.Env.round env = 0 then begin
    let query = (Lazy.force query) in
    (* verify_inlining_count (); *)
    (* safety net: allow inline / unroll up to 30 iteres *)
    let inlining_count =
      try
        Real_closure_origin.Map.find query.function_decl.real_closure_origin
          query.env.inlining_counts
      with Not_found ->
        limit  (* inlining count decremnts with every inlining *)
    in
    if query.env.inlining_level > limit || inlining_count <= 0 then
      Some Data_collector.Action.Apply
    else
      match Random.int 3 with
      | 0 -> None
      | 1 -> Some Data_collector.Action.Inline
      | 2 -> Some Data_collector.Action.Apply
  end else
    None
;;

let () =
  Random.self_init ();
  assert (!Clflags.default_simplify_rounds = 3);  (* Don't use this plugin when not compiling in O3 *)
  (* verify_inlining_count (); *)
  for i = 0 to 2 do
    Printf.eprintf "max_unroll[%d] = %d\n" i (Clflags.Int_arg_helper.get ~key:i !Clflags.inline_max_unroll)
  done;
  Inlining_decision.init_custom_heuristic f
;;
