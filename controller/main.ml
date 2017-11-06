[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

module Config = Protocol.Config
module Info_rpc = Protocol.Info_rpc
module Job_dispatch_rpc = Protocol.Job_dispatch_rpc
module Relpath = Protocol.Relpath
module Execution_stats = Protocol.Execution_stats
module Results = Protocol.Results
module Work_unit_id = Results.Work_unit_id

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

module Make_core_sexp(M :
  sig type t

  val sexp_of_t : t -> Fyp_compiler_lib.Sexp.t
  val t_of_sexp : Fyp_compiler_lib.Sexp.t -> t
end) = struct
  let sexp_of_t t = core_sexp_of_comp_sexp (M.sexp_of_t t)
  let t_of_sexp sexp = M.t_of_sexp (comp_sexp_of_core_sexp sexp)
end

module Data_collector = struct
  include Fyp_compiler_lib.Data_collector
  include Make_core_sexp(Fyp_compiler_lib.Data_collector)
end

module Closure_id = struct
  include Fyp_compiler_lib.Closure_id
  include Make_core_sexp(Fyp_compiler_lib.Closure_id)
end

module Call_site = struct
  include Fyp_compiler_lib.Call_site
  include Make_core_sexp(Fyp_compiler_lib.Call_site)

end

module Call_site_offset = struct
  include Call_site.Offset
  include Make_core_sexp(Call_site.Offset)
end

module Inlining_tree = struct

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
      offset    : Call_site_offset.t;
    }
  and inlined_function =
    { applied   : Closure_id.t;
      offset    : Call_site_offset.t;
      children  : t list;
    }
  [@@deriving sexp]

  let shallow_sexp_of_t sexp =
    let open Sexp in
    match sexp with
    | Declaration decl ->
      List [ Atom "Declaration"; Closure_id.sexp_of_t decl.closure; ]
    | Apply_inlined_function inlined_function ->
      List [
        Atom "Apply_inlined_function";
        Closure_id.sexp_of_t inlined_function.applied;
      ]
    | Apply_non_inlined_function non_inlined_function ->
      List [
        Atom "Apply_non_inlined_function";
        Closure_id.sexp_of_t non_inlined_function.applied;
      ]

  module Top_level = struct
    type nonrec t = t list [@@deriving sexp]
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
    | _ , _ -> false

  let equal_t_and_call_site (t : t) (call_site : Call_site.t) =
    match (t, call_site) with
    | (Declaration decl, Enter_decl enter_decl) ->
      Closure_id.equal decl.closure enter_decl.closure
    | (Apply_inlined_function inlined_function, At_call_site at_call_site) ->
      Closure_id.equal inlined_function.applied at_call_site.applied &&
      Call_site.Offset.equal inlined_function.offset at_call_site.offset

    | (Apply_non_inlined_function non_inlined_function, At_call_site at_call_site) ->
      Closure_id.equal non_inlined_function.applied at_call_site.applied &&
      Call_site.Offset.equal non_inlined_function.offset at_call_site.offset

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

        | Apply_non_inlined_function _ ->
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
                assert (
                  Closure_id.equal collected.applied call_site.applied
                );
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

      | Apply_non_inlined_function _ ->
        failwith "Inconsistent assumption"
    in
    let closure =
      let ident = Fyp_compiler_lib.Ident.create_persistent "<none>" in
      let linkage_name = Fyp_compiler_lib.Linkage_name.create "none" in
      let current_compilation_unit =
        Fyp_compiler_lib.Compilation_unit.create ident linkage_name
      in
      let variable =
        Fyp_compiler_lib.Variable.create ~current_compilation_unit
          "transient"
      in
      Closure_id.wrap variable
    in
    let children = top_level_tree in
    let call_site = Declaration { closure ; children } in
    match
      add__generic call_site (List.rev collected.call_stack)
    with
    | Declaration { closure = _; children } -> children
    | _ -> assert false
  ;;

  let build (decisions : Data_collector.t list) : Top_level.t =
    let (init : Top_level.t) = [] in
    List.fold decisions ~init ~f:add
end

module Traversal_state = struct
  type state =
    | Original
    | Override
  [@@deriving sexp_of]

  type pointer =
    | Root
    | Node_pointer of node_pointer
  and node_pointer =
    { parent  : pointer;
      state   : state;
      node    : Inlining_tree.t;
    }

  let rec sexp_of_pointer (pointer : pointer) =
    match pointer with
    | Root -> Sexp.Atom "Root"
    | Node_pointer node_pointer ->
      Sexp.List [
        Sexp.Atom "Node_pointer";
        sexp_of_node_pointer node_pointer;
      ]
  and sexp_of_node_pointer node_pointer =
    let sexp_of_node (node : Inlining_tree.t) =
      match node with
      | Declaration decl ->
        Sexp.List [
          Sexp.Atom "Declaration";
          Closure_id.sexp_of_t decl.closure;
        ]

      | Apply_inlined_function inlined_function ->
        Sexp.List [
          Sexp.Atom "Apply_inlined_function";
          Closure_id.sexp_of_t inlined_function.applied;
        ]

      | Apply_non_inlined_function non_inlined_function ->
        Sexp.List [
          Sexp.Atom "Apply_non_inlined_function";
          Closure_id.sexp_of_t non_inlined_function.applied;
        ]
    in
    Sexp.List [
      Sexp.List [ Sexp.Atom "parent"; sexp_of_pointer node_pointer.parent ];
      Sexp.List [ Sexp.Atom "state";  sexp_of_state node_pointer.state ];
      Sexp.List [ Sexp.Atom "node";   sexp_of_node node_pointer.node ];
    ]

  type t =
    { pointer   : pointer;
      tree_root : Inlining_tree.Top_level.t;
    }
  [@@deriving sexp_of]

  let rec compute_path pointer ~acc =
    match pointer with
    | Root -> acc
    | Node_pointer node_pointer ->
      compute_path node_pointer.parent ~acc:(node_pointer.node :: acc)

  let change_call_site_source call_site ~source
      : Fyp_compiler_lib.Call_site.t =
    let source = Some source in
    match (call_site : Fyp_compiler_lib.Call_site.t) with
    | Enter_decl enter_decl -> Enter_decl { enter_decl with source }
    | At_call_site acs -> At_call_site { acs with source }

  let node_closure_id = function
    | Inlining_tree.Declaration decl -> decl.closure
    | Apply_inlined_function a -> a.applied
    | Apply_non_inlined_function a -> a.applied

  let to_override_rules t : Fyp_compiler_lib.Data_collector.t list =
    let pointer = t.pointer in
    let rec loop path =
      let match_hd (hd : Inlining_tree.t) ~remainder ~decision
          : (Call_site.t list * bool) =
        match hd with
        | Declaration decl ->
          let source = None in
          let closure = decl.closure in
          (Call_site.Enter_decl { source; closure; } :: remainder,
           decision)
        | Apply_inlined_function inlined_function ->
          let offset = inlined_function.offset in
          let applied = inlined_function.applied in
          let at_call_site =
            { Call_site.
              source = None;
              offset = offset;
              applied = applied;
            }
          in
          (Call_site.At_call_site at_call_site :: remainder, decision)
        | Apply_non_inlined_function non_inlined_function ->
          let offset = non_inlined_function.offset in
          let applied = non_inlined_function.applied in
          let at_call_site =
            { Fyp_compiler_lib.Call_site.
              source = None; offset; applied;
            }
          in
          (Call_site.At_call_site at_call_site :: remainder, decision)
      in
      match (path : Inlining_tree.t list) with
      | hd :: [] ->
        let decision =
          match hd with
          | Declaration _ -> assert false
          | Apply_inlined_function _ -> true
          | Apply_non_inlined_function _ -> false
        in
        match_hd hd ~remainder:[] ~decision
      | hd :: tl ->
        let call_stack, decision = loop tl in
        let source = node_closure_id hd in
        let call_stack =
          change_call_site_source ~source (List.hd_exn call_stack)
          :: List.tl_exn call_stack
        in
        match_hd hd ~remainder:call_stack ~decision
      | [] -> assert false
    in
    let path = compute_path pointer ~acc:[] in
    let call_stack, decision = loop path in
    let module Data_collector = Fyp_compiler_lib.Data_collector in
    let module Acc = struct
      type t =
        { rules       : Data_collector.t list;
          stack_so_far: Call_site.t list;
        }

      let empty = { rules = []; stack_so_far = [] }
    end
    in
    let rec loop_2 ~rev_stack ~(acc : Acc.t) =
      match rev_stack with
      | [] -> acc
      | hd :: tl ->
        let call_stack = acc.stack_so_far @ [ hd ] in
        let new_rule =
          match hd with
          | Call_site.Enter_decl _ -> None
          | Call_site.At_call_site acs ->
            Some (
              let applied = acs.applied in
              match tl with
              | [] -> { Data_collector. call_stack; applied; decision }
              | _  ->
                let decision = true in
                { Data_collector. call_stack; applied; decision }
            )
        in
        let rules =
          match new_rule with
          | None -> acc.Acc.rules
          | Some rule ->  rule :: acc.Acc.rules
        in
        let acc = { Acc. stack_so_far = call_stack; rules; } in
        loop_2 ~rev_stack:tl ~acc
    in
    (*
    Writer.write_sexp (Lazy.force Writer.stdout)
      (List.sexp_of_t (Inlining_tree.shallow_sexp_of_t) path);
    Writer.write (Lazy.force Writer.stdout) "\n";
    Writer.write_sexp (Lazy.force Writer.stdout)
      ([%sexp_of: Call_site.t list] call_stack);
    *)
    (loop_2 ~acc:Acc.empty ~rev_stack:(List.rev call_stack)).Acc.rules
  ;;

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
        | Apply_non_inlined_function _ -> None
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

  let descent_pointer t state =
    let rec loop pointer =
      let match_first_child children =
        match children with
        | [] -> pointer
        | hd :: _ ->
          loop (Node_pointer { parent = pointer; state; node = hd; })
      in
      let match_node node =
        match (node : Inlining_tree.t) with
        | Declaration decl ->
          match_first_child decl.children
        | Apply_inlined_function inlined_function ->
          match_first_child inlined_function.children
        | Apply_non_inlined_function _ ->
          pointer
      in
      match pointer with
      | Root ->
        loop (
          let node = List.hd_exn t.tree_root in
          Node_pointer { parent = Root; state; node }
        )
      | Node_pointer node_pointer -> match_node node_pointer.node
    in
    loop t.pointer

  let refresh_with_new_tree
      (t : t)
      (tree_root : Inlining_tree.Top_level.t) =
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
    let pointer = descent_pointer { pointer; tree_root; } Original in
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
        let pointer = Node_pointer { parent; state = Original; node } in
        let pointer = descent_pointer { t with pointer } Original in
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

  let init (tree_root : Inlining_tree.Top_level.t) =
    let t = { pointer = Root; tree_root; } in
    { t with pointer = descent_pointer t Original }
end

let shell ?(env = []) ?(echo = false) ?(verbose = false) ~dir:working_dir
    prog args =
  Monitor.try_with_or_error (fun () ->
      let env = `Extend env in
      Async_shell.run ~echo ~verbose ~working_dir ~env prog args)
;;

let rec is_prefix ~(prefix : Call_site.t list) (test : Call_site.t list) =
  match prefix, test with
  | [], [] -> false
  | [], _ -> true
  | _, [] -> false
  | prefix_hd :: prefix_tl, hd :: tl ->
    Call_site.equal prefix_hd hd &&
    is_prefix ~prefix:prefix_tl tl

let filter_decisions (decisions : Data_collector.t list) =
  List.filter decisions ~f:(fun test ->
    not (
      List.exists decisions ~f:(fun decision ->
        if phys_equal test decision
        then false
        else (
          not decision.decision &&
          is_prefix ~prefix:(List.rev decision.call_stack) (List.rev test.call_stack)
        )
      )
    )
  )

let lift_deferred m = Deferred.(m >>| fun x -> Core.Or_error.return x)

let get_initial_state ~bin_name ~exp_dir ~base_overrides () =
  shell ~verbose:true ~dir:exp_dir "make" [ "clean" ] >>=? fun () ->
  lift_deferred (
    Writer.save_sexp (exp_dir ^/ "overrides.sexp")
      ([%sexp_of: Data_collector.t list] base_overrides)
  )
  >>=? fun () ->
  shell ~dir:exp_dir "make" [ "all" ] >>=? fun () ->
  (* TODO(fyquah): Run the program in workers to get exec time information.
   *)
  let filename = exp_dir ^/ (bin_name ^ ".0.data_collector.sexp") in
  Reader.load_sexp filename [%of_sexp: Data_collector.t list]
  >>|? fun decisions ->
  match decisions with
  | _ :: _ ->
    let decisions = filter_decisions decisions in
    Some (decisions, Traversal_state.init (Inlining_tree.build decisions))
  | [] -> None
;;

module Worker_connection = struct
  type 'a rpc_conn =
    { socket : ([`Active], 'a) Socket.t;
      reader : Reader.t;
      writer : Writer.t;
      rpc_connection : Rpc.Connection.t
    }

  type ssh_conn = { hostname: string; rundir: string; }

  type 'a t =
    | Rpc : 'a rpc_conn -> 'a t
    | Ssh : ssh_conn -> 'a t
end

let run_binary_on_rpc_worker ~hostname ~worker_connection ~path_to_bin =
  let conn = worker_connection.Worker_connection.rpc_connection in
  Rpc.Rpc.dispatch Info_rpc.rpc conn Info_rpc.Query.Where_to_copy
  >>=? fun path_to_rundir ->
  (* TODO: remove hard code *)
  let abs_path_to_rundir =
    path_to_rundir.Info_rpc.Response.rundir
    ^/ Relpath.to_string (path_to_rundir.Info_rpc.Response.relpath)
  in
  let args = [ path_to_bin; sprintf "%s:%s" hostname abs_path_to_rundir ] in
  shell ~dir:(Filename.dirname path_to_bin) "scp" args
  >>=? fun () ->
  let query =
    Job_dispatch_rpc.Query.Run_binary (
      Relpath.of_string (
        Relpath.to_string path_to_rundir.relpath
        ^/ Filename.basename path_to_bin
      )
    )
  in
  Rpc.Rpc.dispatch Job_dispatch_rpc.rpc conn query
  >>=? fun response ->
  match response with
  | Success results -> Deferred.Or_error.return results
  | Failed e -> Deferred.return (Error e)
;;

let run_binary_on_ssh_worker ~config ~rundir ~hostname ~path_to_bin ~bin_args =
  lift_deferred (Unix.getcwd ())
  >>=? fun dir ->
  shell ~echo:true ~dir "scp"
    [ path_to_bin;
      hostname ^ ":" ^ rundir ^/ "binary.exe";
    ]
  >>=? fun () ->
  Async_shell.run_lines ~echo:true ~working_dir:dir "ssh" [
    hostname;
    rundir ^/ "benchmark_binary.sh";
    rundir ^/ "binary.exe";
    Int.to_string (config.Config.num_runs);
    bin_args;
  ]
  >>= function
  | [] -> Deferred.Or_error.error_s [%message "Something went wrong"]
  | lines ->
    let raw_execution_time =
      List.map lines ~f:(fun line ->
          Time.Span.of_sec (Float.of_string line))
    in
    let worker_hostname = Some hostname in
    Deferred.Or_error.return (
      { Execution_stats. raw_execution_time; worker_hostname; }
    )
;;

let run_binary_on_worker ~config ~hostname ~conn ~path_to_bin ~bin_args =
  match conn with
  | Worker_connection.Rpc worker_connection ->
    run_binary_on_rpc_worker ~worker_connection ~path_to_bin ~hostname
  | Worker_connection.Ssh (ssh_config : Worker_connection.ssh_conn) ->
    let rundir = ssh_config.rundir in
    run_binary_on_ssh_worker ~config ~rundir ~hostname ~path_to_bin ~bin_args
;;

let init_connection ~hostname ~worker_config =
  match worker_config with
  | Protocol.Config.Ssh_worker ssh_config ->
    begin
    lift_deferred (Unix.getcwd ())
    >>=? fun cwd ->
    let args =
      [ "worker/benchmark_binary.sh"; (hostname ^ ":" ^ ssh_config.rundir) ]
    in
    shell ~dir:cwd "scp" args >>|? fun () ->
    let rundir = ssh_config.rundir in
    Worker_connection.Ssh { rundir; hostname; }
    end
  | Protocol.Config.Rpc_worker rpc_config ->
    lift_deferred (
      Tcp.connect
        (Tcp.to_host_and_port hostname rpc_config.port)
        ~timeout:(Time.Span.of_int_sec 1)
    )
    >>=? fun (socket, reader, writer) ->
    Rpc.Connection.create reader writer ~connection_state:(fun _ -> ())
    >>| function
    | Ok rpc_connection ->
      let rpc_connection =
        { Worker_connection. socket; reader; writer; rpc_connection; }
      in
      Ok (Worker_connection.Rpc rpc_connection)
    | Error exn -> raise exn

let rec build_sliding_window ~n input_list =
  if List.length input_list <= n then
    []
  else
    (List.take input_list n)
    :: build_sliding_window ~n (List.tl_exn input_list)
;;

type work_unit =
  { overrides   : Data_collector.t list;
    decisions   : Data_collector.t list;
    path_to_bin : string;
    id          : Results.Work_unit_id.t;
  }

(* - [base_overrides] refers to overrides that should be kept throughout
 *   every compilation -- the sliding window does not include base overrides.
 * - [new_overrides] refers to new overrides to run the sliding window over.
 *   We want to select good overrides here.
 *
 * This funciton assumes that [base_overrides] and [new_overrides] are
 * non-conflicting and mutually exclusive sets of decisions.
 *)
let slide_over_decisions
    ~config ~worker_connections ~bin_name ~exp_dir ~results_dir
    ~base_overrides ~new_overrides ~bin_args =
  let sliding_window = build_sliding_window ~n:2 new_overrides in
  let hostnames =
    List.map ~f:Protocol.Config.hostname config.Config.worker_configs
  in
  let latest_work = Mvar.create () in
  Deferred.don't_wait_for (
    Deferred.Or_error.List.iter ~how:`Sequential sliding_window ~f:(fun decisions ->
        let flipped_decisions =
          List.map decisions ~f:(fun (d : Data_collector.t) ->
              { d with decision = not d.decision })
        in
        shell ~echo:true ~verbose:true ~dir:exp_dir "make" [ "clean" ]
        >>=? fun () ->
        lift_deferred (
          Writer.save_sexp (exp_dir ^/ "overrides.sexp")
            ([%sexp_of: Data_collector.t list]
              (base_overrides @ flipped_decisions))
        )
        >>=? fun () ->
        shell ~verbose:true ~dir:exp_dir "make" [ "all" ]
        >>=? fun () ->
        let filename = Filename.temp_file "fyp-" ("-" ^ bin_name) in
        shell ~echo:true ~verbose:true ~dir:exp_dir
          "cp" [ (bin_name ^ ".native"); filename ]
        >>=? fun () ->
        shell ~echo:true ~dir:exp_dir "chmod" [ "755"; filename ]
        >>=? fun () ->
        Reader.load_sexp (exp_dir ^/ (bin_name ^ ".0.data_collector.sexp"))
          [%of_sexp: Data_collector.t list]
        >>=? fun executed_decisions ->
        let path_to_bin = filename in
        let overrides = flipped_decisions in
        let decisions = executed_decisions in
        let id = Results.Work_unit_id.gen () in
        lift_deferred (
          Mvar.put latest_work (
            Some { path_to_bin; overrides; decisions; id; }
          )
        )
    )
    >>= function
    | Ok () -> Mvar.put latest_work None
    | Error e -> failwithf !"Error %{sexp#hum: Error.t}" e ()
  );
  let results_acc = ref [] in
  Deferred.Or_error.List.iter
    (List.zip_exn worker_connections hostnames)
    ~how:`Parallel ~f:(fun (conn, hostname) ->
      Deferred.repeat_until_finished () (fun () ->
        Mvar.take latest_work
        >>= function
        | None ->
          Mvar.set latest_work None;
          Deferred.return (`Finished (Ok ()))
        | Some work_unit ->
          begin
          let path_to_bin = work_unit.path_to_bin in
          printf "Running %s\n" path_to_bin;
          run_binary_on_worker ~config ~conn ~path_to_bin ~hostname ~bin_args
          >>= function
          | Ok benchmark ->
            printf "Done with %s. Saving results ...\n" path_to_bin;
            Unix.mkdir ~p:() results_dir
            >>= fun () ->
            let work_unit_id = work_unit.id in
            let results =
              let id = work_unit.id in
              let overrides = work_unit.overrides in
              let decisions = work_unit.decisions in
              let path_to_bin = Some path_to_bin in
              { Results. id; benchmark; overrides; decisions; path_to_bin; }
            in
            results_acc := results :: !results_acc;
            let filename =
              results_dir ^/
              (sprintf !"results_%{Results.Work_unit_id}.sexp"
                work_unit_id)
            in
            Writer.save_sexp filename (Results.sexp_of_t results)
            >>| fun () -> `Repeat ()
          | Error e -> Deferred.return (`Finished (Error e))
          end))
    >>|? fun () -> !results_acc
;;

let () =
  let module Generation = struct
    type t =
      { base_overrides : Data_collector.t list;
        gen            : int;
      }
  end
  in
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"Controller"
    [%map_open
     let config_filename =
       flag "-config" (required file) ~doc:"PATH to config file"
     and controller_rundir =
       flag "-rundir" (required string) ~doc:"PATH rundir"
     and exp_dir =
       flag "-exp-dir" (required string) ~doc:"PATH experiment directory"
     and bin_name =
       flag "-bin-name" (required string)
         ~doc:"STRING binary name (without the .ml extension!)"
     and bin_args =
       flag "-args" (required string) ~doc:"STRING arguments"
     in
     fun () ->
       if Filename.check_suffix bin_name ".ml" then begin
         failwith "Binary name should not contain .ml suffix!"
       end;
       (* There is daylight saving now, so UTC timezone == G time zone :D *)
       let exp_uuid =
         Time.to_string_iso8601_basic ~zone:Time.Zone.utc (Time.now ())
       in
       printf "Experiment UUID = %s\n" exp_uuid;
       Reader.load_sexp config_filename [%of_sexp: Config.t]
       >>=? fun config ->
       Deferred.Or_error.List.map config.worker_configs ~how:`Parallel
         ~f:(fun worker_config ->
           let hostname = Protocol.Config.hostname worker_config in
           init_connection ~hostname ~worker_config)
       >>=? fun worker_connections ->

       Deferred.repeat_until_finished { Generation. base_overrides = []; gen = 0 }
         (fun generation ->
           let print_sexp sexp =
             List.iter
               (sexp |> Sexp.to_string_hum |> String.split_lines)
               ~f:(fun line -> printf "[GENERATION %d] %s\n" generation.gen line);
           in
           Log.Global.info ">>>= Running generation %d" generation.gen;
           printf "[GENERATION %d] Starting generation!\n" generation.gen;
           printf "[GENERATION %d] Base overrides:\n" generation.gen;
           print_sexp ([%sexp_of: Data_collector.t list] generation.base_overrides);
           begin
             let base_overrides = generation.base_overrides in
             get_initial_state ~bin_name ~exp_dir ~base_overrides ()
             >>=? fun state ->
             let (new_overrides, _state) = Option.value_exn state in
             let new_overrides =
               (* This filter ensures that every the base and overrides
                * sets are mutually exclusive.
                *
                * Base is definitely non conflicting, as it is generated
                * from the previous generation whereas new_overrides is
                * non conflicting due to the filter_decisions call earlier.
                *)
               List.filter new_overrides ~f:(fun override ->
                 not (
                   List.exists generation.base_overrides ~f:(fun base ->
                     Data_collector.equal base override)
                   )
                 )
             in
             let results_dir =
               controller_rundir
               ^/ exp_uuid
               ^/ Int.to_string_hum generation.gen
             in
             printf "[GENERATION %d] Generation choices:\n" generation.gen;
             print_sexp ([%sexp_of: Data_collector.t list] new_overrides);

             slide_over_decisions ~bin_name ~config ~worker_connections
               ~base_overrides:generation.base_overrides
               ~exp_dir ~results_dir ~new_overrides ~bin_args
           end
           >>| function
           | Ok results ->
             printf "[GENERATION %d] Results summary:\n" generation.gen;
             List.iter results ~f:(fun result ->
               let seconds =
                 Array.of_list_map result.benchmark.raw_execution_time
                   ~f:Time.Span.to_sec
               in
               let mean = Time.Span.of_sec (Owl.Stats.mean seconds) in
               let sd = Time.Span.of_sec (Owl.Stats.std seconds) in
               printf
                 !"[GENERATION %d] Work unit %{Work_unit_id}: %{Time.Span} (sd: %{Time.Span})\n"
                 generation.gen
                 result.id
                 mean
                 sd
             );
             let selected_results = Selection_and_mutate.selection results in
             let old_base = generation.base_overrides in
             begin match
               Selection_and_mutate.mutate ~old_base selected_results
             with
             | None ->
               printf "[GENERATION %d] Terminating at generation %d\n"
                 generation.gen generation.gen;
               `Finished (Ok ())
             | Some new_base ->
               printf
                 "[GENERATION %d] Selected work unit ids for mutation = %s\n"
                 generation.gen
                 (String.concat ~sep:", "
                   (List.map selected_results ~f:(fun result ->
                     (Results.Work_unit_id.to_string result.Results.id))));
               `Repeat (
                 { Generation. gen = generation.gen + 1;
                   base_overrides = new_base;
                 }
               )
             end
           | Error e -> `Finished (Error e))
       ]
  |> Command.run
