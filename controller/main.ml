[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open Core
open Async

module Config = Protocol.Config
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc
module Relpath = Protocol.Relpath

let rec comp_sexp_of_core_sexp core_sexp =
  match core_sexp with
  | Core.Sexp.Atom s -> Fyp_compiler_lib.Sexp.Atom s
  | Core.Sexp.List l ->
    Fyp_compiler_lib.Sexp.List (List.map ~f:comp_sexp_of_core_sexp l)
;;

let rec core_sexp_of_comp_sexp comp_sexp =
  match comp_sexp with
  | Fyp_compiler_lib.Sexp.Atom s -> Core.Sexp.Atom s
  | Fyp_compiler_lib.Sexp.List l ->
    Core.Sexp.List (List.map ~f:core_sexp_of_comp_sexp l)
;;

module Inlining_decision_tree = struct

  module Call_site = Fyp_compiler_lib.Call_site
  module Closure_id = Fyp_compiler_lib.Closure_id
  module Data_collector = Fyp_compiler_lib.Data_collector

  exception Build_error

  type t =
    | Declaration of declaration
    | Apply_inlined_function of inlined_function
    | Apply_non_inlined_function of non_inlined_function
  and declaration =
    { closure   : Closure_id.t;
      children  : t list;
    }
  and non_inlined_function =
    { applied   : Closure_id.t;
      offset    : Call_site.Offset.t;
    }
  and inlined_function =
    { applied   : Closure_id.t;
      offset    : Call_site.Offset.t;
      children  : t list;
    }

  module Top_level = struct
    type nonrec t = t list
  end

  let equal_t_and_call_site (t : t) (call_site : Call_site.t) =
    match (t, call_site) with
    | (Declaration decl, Enter_decl enter_decl) ->
      Closure_id.equal decl.closure enter_decl.closure
    | (Apply_inlined_function inlined_function, At_call_site at_call_site) ->
      Closure_id.equal inlined_function.applied at_call_site.applied &&
      Call_site.Offset.equal inlined_function.offset at_call_site.offset
    | _, _ -> false

  (* Only on short lists! *)
  let rec find_and_replace ~f ~default l =
    match l with
    | [] -> [ default () ]
    | hd :: tl ->
      match f hd with
      | Some a -> a :: tl
      | None -> hd :: (find_and_replace ~f ~default tl)
  ;;

  let add (top_level_tree : Top_level.t) (collected : Data_collector.t) =
    let closure_id = collected.applied in

    let rec add__generic (tree : t) (call_stack : Call_site.t list) =
      match call_stack with
      | [] -> assert false
      | hd :: tl ->
        match hd with
        | At_call_site at_call_site ->
          add__call_site tree at_call_site tl
        | Enter_decl enter_decl ->
          add__declaration tree enter_decl tl

    and add__declaration
        (tree : t)
        (enter_decl : Call_site.enter_decl)
        (call_stack : Call_site.t list) =
      match call_stack with
      | [] -> failwith "Declaration cannot be a leaf node"
      | otherwise ->
        let finder haystack =
          let default () =
            let declaration =
              { children = []; closure = enter_decl.closure; }
            in
            add__generic (Declaration declaration) otherwise
          in
          let f (child : t) =
            if equal_t_and_call_site child
                (Call_site.Enter_decl enter_decl)
            then Some (add__generic child otherwise)
            else None
          in
          find_and_replace haystack ~default ~f
        in
        match tree with
        | Declaration decl ->
          let children = finder decl.children in
          Declaration { decl with children }

        | Apply_inlined_function inlined_function ->
          let children = finder inlined_function.children in
          Apply_inlined_function { inlined_function with children }

        | Apply_non_inlined_function non_inlined_function ->
          failwith "Inconsistent assumption"

    and add__call_site
        (tree : t)
        (call_site : Call_site.at_call_site)
        (call_stack : Call_site.t list) =
      let finder haystack =
        match call_stack with
        | [] ->
          find_and_replace haystack
            ~default:(fun () ->
              if collected.decision then
                let record =
                  { children = [];
                    offset   = call_site.offset;
                    applied  = collected.applied;
                  }
                in
                assert (
                  Closure_id.equal collected.applied call_site.applied
                );
                Apply_inlined_function record
              else
                let record =
                  { offset = call_site.offset;
                    applied = collected.applied;
                  }
                in
                Apply_non_inlined_function record)
            ~f:(fun (child : t) ->
              if equal_t_and_call_site child
                (Call_site.At_call_site call_site)
              then Some child
              else None)
        | otherwise ->
          find_and_replace haystack
            ~default:(fun () ->
              let record =
                { children = [];
                  offset   = call_site.offset;
                  applied  = call_site.applied;
                }
              in
              add__generic (Apply_inlined_function record) otherwise)
            ~f:(fun (child : t) ->
              if equal_t_and_call_site child
                (Call_site.At_call_site call_site)
              then Some (add__generic child otherwise)
              else None)
      in
      match tree with
      | Declaration decl ->
        let children = finder decl.children in
        Declaration { decl with children }

      | Apply_inlined_function inlined_function ->
        let children = finder inlined_function.children in
        Apply_inlined_function { inlined_function with children }

      | Apply_non_inlined_function non_inlined_function ->
        failwith "Inconsistent assumption"
    in
    let closure =
      Closure_id.wrap (Fyp_compiler_lib.Variable.create "transient")
    in
    let children = top_level_tree in
    let call_site = Declaration { closure ; children } in
    match
      add__generic call_site (List.rev collected.call_stack)
    with
    | Declaration { closure = _; children } -> children
    | _ -> assert false
  ;;

  let build (decisions : Data_collector.t list) =
    let (init : Top_level.t) = [] in
    List.fold decisions ~init ~f:add
end

let shell ?(env = []) ?(echo = false) ?(verbose = false) ~dir:working_dir
    prog args =
  Monitor.try_with_or_error (fun () ->
      let env = `Extend env in
      Async_shell.run ~echo ~verbose ~working_dir ~env prog args)
;;

let get_initial_state () =
  let exp_dir = "../experiments/normal/almabench" in
  shell ~dir:exp_dir "make" ["all"]
  >>=? fun () ->
  (* TODO(fyquah): Run the program in workers to get exec time information.
   *)
  let filename = exp_dir ^/ "almabench.0.data_collector.sexp" in
  Reader.load_sexp filename
    (List.t_of_sexp (fun sexp ->
        Fyp_compiler_lib.Data_collector.t_of_sexp
          (comp_sexp_of_core_sexp sexp)))
  >>=? fun decisions ->
  match decisions with
  | hd :: _ ->
    let next_step =
      if hd.decision then Try_no_inline else Try_inline
    in
    let state =
      { Transversal_state.
        rules     = Array.of_list decisions;
        position  = 0;
        next_step = next_step;
        parent    = None
      }
    in
    Some state
  | [] -> None
;;

let () =
  let open Command.Let_syntax in
  Command.async' ~summary:"Controller"
    [%map_open
     let config_filename =
       flag "-filename" (required file) ~doc:"PATH to config file"
     in
     fun () ->
       let open Deferred.Let_syntax in
       let exp_dir = "../experiments/normal/almabench" in
       get_initial_state ()
       >>= fun state ->
       let state = Option.value_exn state in
       Deferred.return ()

       (* 0. Write the relevant overrides.sexp into the exp dir *)

       (* 1. Compile the program *)

       (* 2. Run the program on the target machines. *)

       (* 3. Decide what to explore next *)

       (* 4. Save the compilation data in a directory somewhere. *)

       ]
  |> Command.run


(*

let do_something ~conn =
  let targets =
    { Protocol.Benchmark.
      dir = Relpath.of_string "ocaml-benchs/almabench";
      executable = "almabench.native";
      run_args = [];
    }
  in
  let query =
    { Job_dispatch_rpc.Query.
      compile_params = None;
      targets;
      compiler_selection = Protocol.Compiler_selection.Flambda;
    }
  in
  let%map response =
    Rpc.Rpc.dispatch_exn Job_dispatch_rpc.rpc conn query
  in
  Log.Global.sexp ~level:`Info
    [%message (response : Protocol.Job_dispatch_rpc.Response.t)]
;;

let () =
  let open Command.Let_syntax in
  Command.async' ~summary:"Controller"
    [%map_open
     let config_filename =
       flag "-filename" (required file) ~doc:"PATH to config file"
     in
     fun () ->
       let open Deferred.Let_syntax in
       let%bind config =
         Reader.load_sexp_exn config_filename [%of_sexp: Config.t]
       in
       let worker_host = "0.0.0.0" in
       let worker_port = config.worker_port in
       Tcp.with_connection
         (Tcp.to_host_and_port worker_host worker_port)
         ~timeout:(Time.Span.of_int_sec 1)
         (fun _ r w ->
            match%bind
              Rpc.Connection.create r w ~connection_state:(fun _ -> ())
            with
            | Error exn -> raise exn
            | Ok conn -> do_something conn)]
  |> Command.run

*)

(*
module Transversal_state = struct
  type next_step =
    | Try_inline
    | Try_no_inline
    | Next_override
  [@@deriving sexp]

  type t =
    { rules     : Fyp_compiler_lib.Data_collector.t Array.t;
      position  : int
      next_step : next_step;
      parent    : t option;
    }

  let call_site_at_same_function a closure_id =
    match a with
    | At_call_site a ->
      Option.equal Fyp_compiler_lib.Closure_id.equal
        a.closure_id (Some closure_id)
    | _ -> false

  let backtrack_and_step t =
    if t.position >= Array.length t.rules - 1 then
      begin match t.parent with
      | None   -> None
      | Some s -> backtrack_and_step s
      end
    else
      Some { t with position = t.position + 1 }
  ;;

  let next t (decisions : Data_collector.t list) =
    match t.next with
    | Try_inline ->
      let rule = t.rules.(t.position) in
      begin match
        List.find decisions ~f:(fun decision ->
          let hd = List.hd_exn decision.call_stack in
          List.equal ~equal:Call_site.equal (List.tl_exn decision.call_stack)
          && call_site_at_same_function hd rule.applied)
      with
      | Some a ->
      | None ->
        (* Possible when there is no functions in that underlying function
         *  to inline further
         *)
        backtrack_and_step t
      end
    | Try_no_inline -> f flag
end
*)
