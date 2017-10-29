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

module Inlining_tree = struct

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

  let fuzzy_equal a b =
    match a, b with
    | Declaration a, Declaration b ->
      Closure_id.equal a.closure b.closure
    | Apply_inlined_function a, Apply_inlined_function b ->
      Closure_id.equal a.applied a.applied &&
      Call_site.Offset.equal a.offset b.offset
    | Apply_non_inlined_function a, Apply_non_inlined_function b ->
      Closure_id.equal a.applied a.applied &&
      Call_site.Offset.equal a.offset b.offset
    | Apply_inlined_function a, Apply_non_inlined_function b ->
      Closure_id.equal a.applied a.applied &&
      Call_site.Offset.equal a.offset b.offset
    | Apply_non_inlined_function b, Apply_non_inlined_function a ->
      Closure_id.equal a.applied a.applied &&
      Call_site.Offset.equal a.offset b.offset
    | _ , _ -> false

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

module Traversal_state = struct
  type state =
    | Original
    | Override

  type pointer =
    | Root
    | Node_pointer of node_pointer
  and node_pointer =
    { parent  : pointer;
      state   : state;
      node    : Inlining_tree.t;
    }

  type t =
    { pointer   : pointer;
      tree_root : Inlining_tree.Top_level.t;
    }

  let rec compute_path pointer ~acc =
    match pointer with
    | Root -> acc
    | Node_pointer node_pointer ->
      compute_path node_pointer.parent ~acc:(node_pointer.node :: acc)

  let refresh_tree_path
      (tree_root : Inlining_tree.Top_level.t)
      (path : Inlining_tree.t list) =
    let rec find_path_in_node
        (tree : Inlining_tree.t)
        (path : Inlining_tree.t list) =
      match path with
      | [] -> Some [ tree ]
      | hd :: tl ->
        match tree with
        | Apply_non_inlined_function non_inlined_function -> None
        | Apply_inlined_function { children; _ }
        | Declaration { children; _ } ->
          let found =
            List.find_exn children ~f:(Inlining_tree.fuzzy_equal hd)
          in
          Option.map ~f:(fun a -> tree :: a) (find_path_in_node found tl)
    in
    match path with
    | [] -> failwith "Cannot match an empty path!"
    | otherwise ->
      List.find_map_exn tree_root ~f:(fun child ->
        find_path_in_node child otherwise)

  let descent_pointer pointer state =
    let rec loop pointer =
      let match_first_child children =
        match children with
        | [] -> pointer
        | hd :: _ ->
          loop (Node_pointer { parent = pointer; state; node = hd; })
      in
      match pointer with
      | Root -> failwith "Root"
      | Node_pointer node_pointer ->
        match node_pointer.node with
        | Declaration decl ->
          match_first_child decl.children
        | Apply_inlined_function inlined_function ->
          match_first_child inlined_function.children
        | Apply_non_inlined_function non_inlined_function ->
          pointer
    in
    loop pointer

  let refresh_with_new_tree (t : t) (tree_root : Inlining_tree.Top_level.t) =
    let pointer = t.pointer in
    let path = compute_path pointer ~acc:[] in
    let refreshed_tree_path = refresh_tree_path tree_root path in
    let rec refresh_pointer pointer rpath =
      match pointer , rpath with
      | Root, [] -> Root
      | Node_pointer node_pointer, hd :: tl ->
        let new_parent = refresh_pointer pointer tl in
        Node_pointer { node_pointer with node = hd; parent = new_parent; }
      | Root, _ -> failwith "Shouldn't happen"
      | Node_pointer _, [] -> failwith "Shouldn't happen"
    in
    let pointer = refresh_pointer pointer (List.rev refreshed_tree_path) in
    let bottom_node = List.tl_exn refreshed_tree_path in
    let pointer = descent_pointer pointer Original in
    { pointer ; tree_root }
  ;;

  let flip_node (node : Inlining_tree.t) : Inlining_tree.t =
    match node with
    | Declaration _ -> failwith "Cannot flip a declaration node"
    | Apply_inlined_function { applied; offset } ->
      Apply_non_inlined_function { Inlining_tree. applied; offset }
    | Apply_non_inlined_function { applied; offset; _ } ->
      Apply_inlined_function { applied; offset; children = [] }

  let rec backtrack t =
    match t.pointer with
    | Root -> None
    | Node_pointer node_pointer ->
      let siblings =
        match node_pointer.parent with
        | Root -> t.tree_root
        | Node_pointer node_pointer ->
          match node_pointer.node with
          | Declaration d -> d.children
          | Apply_inlined_function inlined_function -> inlined_function.children
          | Apply_non_inlined_function _ ->
            failwith "Shouldn't have arrived here"
      in
      let index, (_ : Inlining_tree.t) =
        Option.value_exn (
          List.findi ~f:(fun (_ : int) a ->
              match t.pointer with
              | Node_pointer node_pointer ->
                Inlining_tree.fuzzy_equal a node_pointer.node
              | Root -> false)
            siblings
        )
      in
      if index >= List.length siblings - 1
      then begin
        match node_pointer.parent with
        | Root -> None
        | Node_pointer parent_node_pointer ->
          match parent_node_pointer.node, parent_node_pointer.state with
          | Declaration _, _
          | _, Override ->
            backtrack
              { t with pointer = Node_pointer parent_node_pointer }
          | node, Original ->
            let node = flip_node node in
            let pointer =
              Node_pointer { node_pointer with state = Override; node }
            in
            Some { t with pointer }
      end else
        let node = List.nth_exn siblings (index + 1) in
        let parent = node_pointer.parent in
        (* TODO fyquah: descent here ? *)
        let pointer =
          descent_pointer
            (Node_pointer { parent; state = Original; node })
            Original
        in
        let pointer =
          match pointer with
          | Node_pointer node_pointer ->
            Node_pointer { node_pointer with state = Override }
          | Root -> assert false
        in
        Some { t with pointer }

  let step (t : t) =
    let pointer = t.pointer in
    match pointer with
    | Root -> None
    | Node_pointer node_pointer ->
      match node_pointer.state with
      | Original ->
        (* The leaf node we are looking at has just been generated,
         * so we should move it to an override state to explore possible
         * inlining opportunities there.
         *)
        let node = flip_node node_pointer.node in
        let pointer =
          Node_pointer { node_pointer with state = Override; node }
        in
        Some { t with pointer }

      | Override ->
        (* Our previous run was overiding this node. It is possible that
         * we are still here because there is no further call sites
         * beneath. This means we should backtrack.
         *)
        backtrack t
  ;;

  let next (t : t) (new_tree_root : Inlining_tree.Top_level.t) =
    step (refresh_with_new_tree t new_tree_root)
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

