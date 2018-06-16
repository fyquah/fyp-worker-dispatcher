[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let save_sexp_to_file ~output_prefix ~version sexp =
  let filename = output_prefix ^ ".data_collector." ^ version ^ ".sexp" in
  let out_channel = open_out filename in
  let ppf = Format.formatter_of_out_channel out_channel in
  Format.fprintf ppf "%a" Sexp.print_mach sexp;
  close_out out_channel
;;

module V0 = struct

  (* This is wrapped in a module to guranteed principality *)
  module Query = struct
    type t =
      { call_stack: Call_site.t list;
        applied: Closure_id.t;
      }
  end

  type t =
    { call_stack : Call_site.t list;
      applied : Closure_id.t;
      decision : bool;
    }

  let (inlining_decisions : t list ref) = ref []

  let sexp_of_t t =
    Sexp.List [
      Sexp.sexp_of_list Call_site.sexp_of_t t.call_stack;
      Closure_id.sexp_of_t t.applied;
      Sexp.t_of_bool t.decision;
    ]

  let t_of_sexp sexp =
    match sexp with
    | Sexp.List (call_stack :: applied :: decision :: []) ->
      let call_stack = Sexp.list_of_sexp Call_site.t_of_sexp call_stack in
      let applied = Closure_id.t_of_sexp applied in
      let decision = Sexp.bool_of_t decision in
      { call_stack; applied; decision; }
    | otherwise ->
      raise (Sexp.Parse_error (
        Format.asprintf "Cannot parse %a as a Data_collector.t"
          Sexp.print_mach otherwise))


  let save ~output_prefix =
    let sexp = Sexp.sexp_of_list sexp_of_t !inlining_decisions in
    save_sexp_to_file ~version:"v0" ~output_prefix sexp
  ;;

  let load_from_channel ic =
    Sexp.list_of_t t_of_sexp (Sexp_file.load_from_channel ic)
  ;;

  let pprint_list ppf ts =
    List.iter
      (fun t -> Format.fprintf ppf "=> %a\n" Sexp.print_mach (sexp_of_t t))
      ts
  ;;

  let equal a b =
    a.decision = b.decision &&
    Closure_id.partial_equal a.applied b.applied &&
    Helper.list_equal Call_site.equal a.call_stack b.call_stack

  let find_decision overrides ~call_stack ~applied =
    match
      List.find_opt (fun a ->
          Closure_id.equal a.applied applied
          && Helper.list_equal Call_site.equal a.call_stack call_stack)
        overrides
    with
    | None -> None
    | Some a -> Some a.decision
  ;;
end

module Option = struct
  let t_of_sexp f = function
    | Sexp.List [] -> None
    | Sexp.List [a] -> Some (f a)
    | _ -> raise (Sexp.Parse_error "Option.t_of_sexp failed to pattern match!")
  ;;

  let sexp_of_t f = function
    | None -> Sexp.List []
    | Some x -> Sexp.List [f x]
  ;;

  let equal f a b =
    match a, b with
    | None, None -> true
    | Some a, Some b -> f a b
    | _, _ -> false
end


module V1 = struct
  module Function_metadata = struct
    (* TODO(fyq14): Make [Set_of_closure_id.t_of_sexp] work.
     *
     * It is okay (in the context of early experiments) not to include that,
     * but in future versions it can give a lot of useful information about
     * recursive functions.
     *
     * *)

    type t =
      { closure_id: Closure_id.t option;
        set_of_closures_id: Set_of_closures_id.t option;
        closure_origin: Closure_origin.t;
        opt_closure_origin: Closure_origin.t option;
        specialised_for : Apply_id.t option;
      }

    let unknown =
      let closure_id = None in
      let set_of_closures_id = None in
      let closure_origin = Closure_origin.unknown in
      let opt_closure_origin = None in
      let specialised_for = None in
      { closure_id; set_of_closures_id; closure_origin; opt_closure_origin;
        specialised_for;
      }
    ;;

    let compare a b = Closure_origin.compare a.closure_origin b.closure_origin

    let sexp_of_t t =
      Sexp.List [
        Option.sexp_of_t Closure_id.sexp_of_t t.closure_id;
        Option.sexp_of_t Set_of_closures_id.sexp_of_t None;
        Closure_origin.sexp_of_t t.closure_origin;
        Option.sexp_of_t Closure_origin.sexp_of_t t.opt_closure_origin;
        Option.sexp_of_t Apply_id.sexp_of_t t.specialised_for;
      ]
    ;;

    let print ppf t = Sexp.print_mach ppf (sexp_of_t t)

    let t_of_sexp sexp =
      let open Sexp in
      match sexp with
      | List [closure_id; _set_of_closures_id; closure_origin; opt_closure_origin; specialised_for; ] ->
        let closure_id =
          Option.t_of_sexp Closure_id.t_of_sexp closure_id
        in
        let set_of_closures_id = None in  (* TODO(fyq14): Actually import this *)
        let closure_origin = Closure_origin.t_of_sexp closure_origin in
        let opt_closure_origin =
          Option.t_of_sexp Closure_origin.t_of_sexp opt_closure_origin
        in
        let specialised_for =
          Option.t_of_sexp Apply_id.t_of_sexp specialised_for
        in
        { closure_id; set_of_closures_id; closure_origin; opt_closure_origin; specialised_for; }

      (* Parses the version after proof, but before specialised_for *)
      | List [closure_id; _set_of_closures_id; closure_origin; opt_closure_origin] ->
        let closure_id =
          Option.t_of_sexp Closure_id.t_of_sexp closure_id
        in
        let set_of_closures_id = None in  (* TODO(fyq14): Actually import this *)
        let closure_origin = Closure_origin.t_of_sexp closure_origin in
        let opt_closure_origin =
          Some (Closure_origin.t_of_sexp opt_closure_origin)
        in
        let specialised_for = None in
        { closure_id; set_of_closures_id; closure_origin; opt_closure_origin; specialised_for; }

      (* Parses the version before proof and specialised_for *)
      | List [closure_id; _set_of_closures_id; closure_origin] ->
        let closure_id =
          Option.t_of_sexp Closure_id.t_of_sexp closure_id
        in
        let set_of_closures_id = None in  (* TODO(fyq14): Actually import this *)
        let closure_origin = Closure_origin.t_of_sexp closure_origin in
        let opt_closure_origin = None in
        let specialised_for = None in
        { closure_id; set_of_closures_id; closure_origin; opt_closure_origin; specialised_for; }
      | _ -> raise (Sexp.Parse_error "oops")
    ;;
  end

  module Trace_item = struct
    open Sexp

    (* TODO(fyq14): Isn't [source] really just redundant? *)

    type enter_decl =
      { source: Function_metadata.t option;
        declared: Function_metadata.t;
      }

    let sexp_of_enter_decl { source; declared; } =
      List [
        Option.sexp_of_t Function_metadata.sexp_of_t source;
        Function_metadata.sexp_of_t declared;
      ]
    ;;

    let enter_decl_of_sexp = function
      | List [ source; declared; ] ->
        { source = Option.t_of_sexp Function_metadata.t_of_sexp source;
          declared = Function_metadata.t_of_sexp declared;
        }
      | _ -> raise (Parse_error "oops")
    ;;

    type at_call_site =
      { source: Function_metadata.t option;
        apply_id: Apply_id.t;
        applied: Function_metadata.t;
      }

    let sexp_of_at_call_site { source; apply_id; applied } =
      List [
        Option.sexp_of_t Function_metadata.sexp_of_t source;
        Apply_id.sexp_of_t apply_id;
        Function_metadata.sexp_of_t applied;
      ]

    let at_call_site_of_sexp = function
      | List [a; b; c] ->
        let source = Option.t_of_sexp Function_metadata.t_of_sexp a in
        let apply_id = Apply_id.t_of_sexp b in
        let applied = Function_metadata.t_of_sexp c in
        { source; apply_id; applied; }
      | _ -> raise (Sexp.Parse_error "oops")
    ;;

    type t =
      | Enter_decl of enter_decl
      | At_call_site of at_call_site

    let sexp_of_t = function
      | Enter_decl x ->
        Sexp.List [
          Sexp.Atom "Enter_decl";
          sexp_of_enter_decl x
        ]
      | At_call_site x ->
        Sexp.List [
          Sexp.Atom "At_call_site";
          sexp_of_at_call_site x;
        ]

    let t_of_sexp sexp =
      let open Sexp in
      match sexp with
      | List [ Atom "Enter_decl"; x ] ->
        Enter_decl (enter_decl_of_sexp x)
      | List [ Atom "At_call_site"; x ] ->
        At_call_site (at_call_site_of_sexp x)
      | _ -> raise (Sexp.Parse_error "oops")

    let pprint ppf t =
      match t with
      | Enter_decl enter_decl ->
        Format.fprintf ppf "Enter_decl(%a)"
          Closure_origin.print enter_decl.declared.closure_origin
      | At_call_site at_call_site ->
        Format.fprintf ppf "At_call_site[%a](%a)"
          Apply_id.print at_call_site.apply_id
          Closure_origin.print at_call_site.applied.closure_origin
    ;;

    let minimally_equal a b =
      match a, b with
      | Enter_decl a, Enter_decl b ->
        let a = a.declared.closure_origin in
        let b = b.declared.closure_origin in
        Compilation_unit.equal (Closure_origin.get_compilation_unit a)
          (Closure_origin.get_compilation_unit b)
        && String.equal (Closure_origin.get_name a) (Closure_origin.get_name b)

      | At_call_site a, At_call_site b ->
        Apply_id.equal a.apply_id b.apply_id

      | _ -> false
    ;;

    let get_apply_id_exn = function
      | Enter_decl _ -> Misc.fatal_error "no"
      | At_call_site a -> a.apply_id
    ;;
  end

  (* What we decided to do at the call site, using this rather than a
   * boolean to reflect:
   *   - the "MDP"-like behaviour (argued in the interim report)
   *   - it is possible that we want to support more than Inline and Apply,
   *     of which we won't need to make a new version just for that (as
   *     sexp decoding will be similar)
   *)
  module Action = struct
    type t =
      | Inline
      | Specialise
      | Apply  (* Aka do nothing *)

    let sexp_of_t = function
      | Specialise -> Sexp.Atom "Specialise"
      | Inline -> Sexp.Atom "Inline"
      | Apply -> Sexp.Atom "Apply"

    let t_of_sexp = function
      | Sexp.Atom "Specialise" -> Specialise
      | Sexp.Atom "Inline" -> Inline
      | Sexp.Atom "Apply" -> Apply
      | _ -> raise (Sexp.Parse_error "oops")
  end

  module Decision = struct
    type t =
      { round:    int;
        trace:    Trace_item.t list;
        apply_id: Apply_id.t;
        action:   Action.t;
        metadata: Function_metadata.t;
      }

    let sexp_of_t { round; trace; apply_id; action; metadata; } =
      Sexp.List [
        Sexp.Atom (string_of_int round);
        Sexp.List (List.map Trace_item.sexp_of_t trace);
        Apply_id.sexp_of_t apply_id;
        Action.sexp_of_t action;
        Function_metadata.sexp_of_t metadata;
      ]

    let t_of_sexp = function
      | Sexp.List [ Atom round; Sexp.List trace; apply_id; action; metadata; ] ->
        let round = int_of_string round in
        let trace = List.map Trace_item.t_of_sexp trace in
        let apply_id = Apply_id.t_of_sexp apply_id in
        let action = Action.t_of_sexp action in
        let metadata = Function_metadata.t_of_sexp metadata in
        { round; trace; apply_id; action; metadata; }
      | _ -> raise (Sexp.Parse_error "oops")

    let (recorded_from_flambda : t list ref) = ref []

    let save ~output_prefix =
      let sexp = Sexp.sexp_of_list sexp_of_t !recorded_from_flambda in
      save_sexp_to_file ~output_prefix ~version:"v1" sexp
    ;;

  end

  module Overrides = struct

    type t = Decision.t list

    type query = {
      round : int;
      trace: Trace_item.t list;
      apply_id: Apply_id.t;
    }

    let trace_to_string trace =
      List.map (function
          | Trace_item.Enter_decl decl ->
            Format.asprintf "{%a}" Closure_origin.print decl.declared.closure_origin
          | Trace_item.At_call_site acs ->
            Format.asprintf "<%a(%a)>" Apply_id.print acs.apply_id
              Closure_origin.print acs.applied.closure_origin
        )
        trace
      |> String.concat "/"
    ;;

    let action_to_string action =
      match action with
      | Action.Inline -> "INLINE"
      | Action.Apply -> "DONT_INLINE"
      | Action.Specialise -> "SPECIALISE"
    ;;

    let find_decision overrides ({ trace; apply_id; round; }) =
      match
        List.find_opt (fun (decision : Decision.t) ->
            Apply_id.equal_accounting_deprecation decision.apply_id apply_id
            && Helper.list_equal Trace_item.minimally_equal decision.trace trace
            && (decision.round < 0 || decision.round = round))
          overrides
      with
      | None ->
        Format.printf "NONE  -- %s\n" (trace_to_string trace);
        None
      | Some (a : Decision.t) ->
        Format.printf "%s  -- %s\n" (action_to_string a.action) (trace_to_string trace);
        Some a.action
    ;;

    let sexp_of_t t =
      Sexp.List (List.map Decision.sexp_of_t t)
    ;;

    let t_of_sexp = function
      | Sexp.List decisions -> List.map Decision.t_of_sexp decisions
      | _ -> raise (Sexp.Parse_error "oops")
    ;;

    let of_decisions t = t

    exception Tree_error

    type node =
      | Decl of {
        declared : Function_metadata.t;
        children : node list
      }
      | Apply of Apply_id.t
      | Inlined of {
        apply_id : Apply_id.t;
        children : node list
      }
    ;;

    type root = node list

    let equal_node_and_trace_item (node : node) (trace_item : Trace_item.t) =
      match (node, trace_item) with
      | (Decl decl, Enter_decl enter_decl) ->
        Closure_origin.equal decl.declared.closure_origin
          enter_decl.declared.closure_origin

      | (Inlined inlined, At_call_site at_call_site) ->
        Apply_id.equal inlined.apply_id at_call_site.apply_id

      | (Apply apply_id, At_call_site at_call_site) ->
        Apply_id.equal apply_id at_call_site.apply_id

      | _, _ -> false
    ;;


    (* Only on short lists! *)
    let rec find_and_replace ~f ~default l =
      match l with
      | [] -> [ default () ]
      | hd :: tl ->
        match f hd with
        | Some a -> a :: tl
        | None -> hd :: (find_and_replace ~f ~default tl)
    ;;

    let add (top_level_tree : root) (collected : Decision.t) =

      let rec add__generic (tree : node) (call_stack : Trace_item.t list) =
        match call_stack with
        | [] -> assert false
        | hd :: tl ->
          match hd with
          | At_call_site at_call_site ->
            add__call_site tree at_call_site tl
          | Enter_decl enter_decl ->
            add__declaration tree enter_decl tl

      and add__declaration
          (tree : node)
          (enter_decl : Trace_item.enter_decl)
          (call_stack : Trace_item.t list) =
        match call_stack with
        | [] -> raise Tree_error
        | otherwise ->
          let finder haystack =
            let default () =
              let declaration =
                Decl { children = []; declared = enter_decl.declared; }
              in
              add__generic declaration otherwise
            in
            let f (child : node) =
              if equal_node_and_trace_item child
                  (Trace_item.Enter_decl enter_decl)
              then Some (add__generic child otherwise)
              else None
            in
            find_and_replace haystack ~default ~f
          in
          match tree with
          | Decl decl ->
            let children = finder decl.children in
            Decl { decl with children }

          | Inlined inlined_function ->
            let children = finder inlined_function.children in
            Inlined { inlined_function with children }

          | Apply _ ->
            tree
            (* It is possible, due to the nature of the [inline] function. It
             * inlines the contents of the function body before trying to
             * deciding to inline the function itself.
             *
             * In cases as such, the parent takes higher priority (which should
             * contain an equally pessimistic or more pessimistic decision).
             *)

      and add__call_site
          (tree : node)
          (call_site : Trace_item.at_call_site)
          (call_stack : Trace_item.t list) =
        let finder haystack =
          match call_stack with
          | [] ->
            find_and_replace haystack
              ~default:(fun () ->
                match collected.action with
                | Action.Specialise -> assert false
                | Action.Inline ->
                  Inlined {
                    children = [];
                    apply_id = call_site.apply_id;
                  }
                | Action.Apply -> Apply call_site.apply_id)
              ~f:(fun (child : node) ->
                if equal_node_and_trace_item child
                  (Trace_item.At_call_site call_site)
                then Some child
                else None)
          | otherwise ->
            find_and_replace haystack
              ~default:(fun () ->
                let record =
                  Inlined {
                    children = []; apply_id   = call_site.apply_id;
                  }
                in
                add__generic record otherwise)
              ~f:(fun (child : node) ->
                if equal_node_and_trace_item child
                  (Trace_item.At_call_site call_site)
                then Some (add__generic child otherwise)
                else None)
        in
        match tree with
        | Decl decl ->
          let children = finder decl.children in
          Decl { decl with children }

        | Inlined inlined_function ->
          let children = finder inlined_function.children in
          Inlined { inlined_function with children }

        | Apply _ ->
          tree
      in
      let declared =
        let closure_origin = Closure_origin.unknown in
        { Function_metadata.
          closure_id = None;
          set_of_closures_id = None;
          closure_origin;
          opt_closure_origin = None;
          specialised_for = None;
        }
      in
      let children = top_level_tree in
      let call_site = Decl { declared ; children } in
      match
        add__generic call_site (List.rev collected.trace)
      with
      | Decl { declared = _; children } -> children
      | _ -> assert false
    ;;

    let rec recursively_reverse children =
      let reverse_node node =
        match node with
        | Decl decl ->
          Decl { decl with children = recursively_reverse decl.children }
        | Inlined inlined ->
          Inlined {
            inlined with children = recursively_reverse inlined.children
          }
        | Apply _ -> node
      in
      List.rev_map reverse_node children
    ;;

    let build_tree (decisions : Decision.t list) =
      let init = [] in
      List.filter (fun (decision : Decision.t) ->
          match decision.action with
          | Action.Specialise -> false
          | Action.Inline -> true
          | Action.Apply -> true)
        decisions
      |> List.fold_left add init
      |> recursively_reverse
    ;;

    let rec check_prefix ~equal ~prefix path =
      match prefix, path with
      | [], _ -> true
      | _, [] -> false
      | (prefix_hd :: prefix_tl), (path_hd :: path_tl) ->
        equal prefix_hd path_hd
        && check_prefix ~equal ~prefix:prefix_tl path_tl
    ;;

    let is_decl = function
      | Decl _ -> true
      | _ -> false
    ;;

    let get_apply_id_exn = function
      | Decl _ -> Misc.fatal_error "Cannot [get_apply_id_exn] on decl"
      | Inlined { apply_id; _ }
      | Apply apply_id -> apply_id
    ;;

    (* TODO(fyq14): This _assumes_ that the tree does not contain any stubs. *)
    let find_in_tree =
      let remove_stub_components path =
        List.filter (fun (_cu, a) ->
            match a with
            | Apply_id.Stub -> false
            | _ -> true)
          path
      in
      let try_to_match (trace_item : Trace_item.t) (node : node) =
        match (node, trace_item) with
        | (Decl decl, Enter_decl enter_decl) ->
          Closure_origin.equal decl.declared.closure_origin
            enter_decl.declared.closure_origin

        | (Inlined { apply_id; _ }, At_call_site at_call_site)
        | (Apply apply_id, At_call_site at_call_site) ->
          let reference_path = Apply_id.get_inlining_path apply_id in
          let call_site_path =
            Apply_id.get_inlining_path at_call_site.apply_id
          in
          let equal (cu_a, s_a) (cu_b, s_b) =
            Compilation_unit.equal cu_a cu_b &&
            (Apply_id.compare_stamp s_a s_b = 0)
          in
          check_prefix ~equal ~prefix:(remove_stub_components reference_path)
            (remove_stub_components call_site_path)

        | _, _ -> false
      in
      let rec find ~subtree query =
        match query with
        | [] -> None
        | hd :: tl ->
          begin match List.find_opt (try_to_match hd) subtree with
          | None -> None
          | Some found ->
            let subtree =
              match found with
              | Inlined { children; _ } -> children
              | Decl { children; _ } -> children
              | Apply _ -> []
            in
            let move_forward () =
              begin match tl with
              | [] ->
                begin match found with
                | Inlined _ -> Some Action.Inline
                | Apply _ -> Some Action.Apply
                | _ -> None
                end
              | otherwise -> find ~subtree otherwise
              end
            in
            if is_decl found then
              move_forward ()
            else begin
              let forward_path =
                Apply_id.get_inlining_path (get_apply_id_exn found)
              in
              let trace_item_path =
                Apply_id.get_inlining_path (Trace_item.get_apply_id_exn hd)
              in
              if List.length forward_path = List.length trace_item_path then
                move_forward ()
              else if List.length forward_path < List.length trace_item_path then
                find ~subtree query
              else
                (* assert false *)
                None
            end
          end
      in
      fun tree query ->
        (* Query uses [list] as a stack *)
        let trace =
          List.filter (fun item ->
              match item with
              | Trace_item.Enter_decl _ -> true
              | Trace_item.At_call_site acs ->
                match acs.apply_id.stamp with
                | Apply_id.Stub -> false
                | _ -> true)
            query.trace
        in
        find ~subtree:tree (List.rev trace)
    ;;

    let print_node ppf node =
      match node with
      | Decl decl -> 
        Format.fprintf ppf "Decl(%a)"
          Closure_origin.print decl.declared.closure_origin
      | Inlined inlined ->
        Format.fprintf ppf "Inlined[%a]" Apply_id.print inlined.apply_id
      | Apply apply_id ->
        Format.fprintf ppf "Apply[%a]" Apply_id.print apply_id
    ;;

    let get_children = function
      | Decl { children; _ }
      | Inlined { children; _ } -> children
      | Apply _ -> []
    ;;

    let print_tree ppf  =
      let rec loop ~depth tree =
        let space = String.make (depth * 2) ' ' in
        List.iter (fun child ->
            Format.fprintf ppf "%s%a\n" space print_node child;
            loop ~depth:(depth + 1) (get_children child))
          tree
      in
      fun tree ->
        loop ~depth:0 tree
    ;;

    let _ = print_tree
  end
end

include V1

module Simple_overrides = struct

  type decl = {
    linkage_name : Linkage_name.t;
    name         : string;
  }

  type apply = {
    linkage_name : Linkage_name.t;
    stamp        : int option;
  }

  type trace_item =
    | Apply of apply
    | Decl of decl

  let eq_v1_trace_item (trace_item : trace_item) v1_trace_item =
    match trace_item, v1_trace_item with
    | Apply a, V1.Trace_item.At_call_site acs ->
      let acs_linkage_name =
        Compilation_unit.get_linkage_name (
          Apply_id.get_compilation_unit acs.apply_id
        )
      in
      let acs_stamp = Apply_id.get_stamp acs.apply_id.stamp in
      Option.equal (=) a.stamp  acs_stamp
      && Linkage_name.equal a.linkage_name acs_linkage_name
    | Decl d, V1.Trace_item.Enter_decl enter_decl ->
      let enter_decl_linkage_name =
        Compilation_unit.get_linkage_name (
          Closure_origin.get_compilation_unit (
            enter_decl.declared.closure_origin
          )
        )
      in
      let enter_decl_name =
        Closure_origin.get_name enter_decl.declared.closure_origin
      in
      String.equal d.name enter_decl_name
      && Linkage_name.equal d.linkage_name enter_decl_linkage_name
    | _ -> false
  ;;

  type entry = {
    round:           int;
    apply_id_stamp:  int option;
    trace:           trace_item list;
    action:          Action.t;
  }

  type t = entry list

  let decl_of_sexp sexp =
    let open Sexp in
    match sexp with
    | List [ linkage_name; name ] ->
      let linkage_name = Linkage_name.t_of_sexp linkage_name in
      let name = Sexp.string_of_t name in
      { linkage_name; name; }
    | _ -> raise (Sexp.Parse_error "bla")
  ;;

  let apply_of_sexp sexp =
    let open Sexp in
    match sexp with
    | List [ linkage_name; stamp; ] ->
      let linkage_name = Linkage_name.t_of_sexp linkage_name in
      let stamp = Option.t_of_sexp Sexp.int_of_t stamp in
      { linkage_name; stamp; }
    | _ -> raise (Sexp.Parse_error "bla")
  ;;

  let sexp_of_apply a =
    let { linkage_name; stamp; } = a in
    Sexp.List [
      Linkage_name.sexp_of_t linkage_name;
      Option.sexp_of_t Sexp.t_of_int stamp;
    ]
  ;;

  let sexp_of_decl d =
    let { linkage_name; name; } = d in
    Sexp.List [
      Linkage_name.sexp_of_t linkage_name;
      Sexp.t_of_string name;
    ]
  ;;

  let sexp_of_trace_item ti =
    let open Sexp in
    match ti with
    | Apply a -> List [ Atom "Apply"; sexp_of_apply a; ]
    | Decl d  -> List [ Atom "Decl"; sexp_of_decl d; ]
  ;;

  let trace_item_of_sexp sexp =
    let open Sexp in
    match sexp with
    | List [ Atom "Apply"; x ] -> Apply (apply_of_sexp x)
    | List [ Atom "Decl"; x ] -> Decl (decl_of_sexp x)
    | _ -> raise (Sexp.Parse_error "bla")
  ;;

  let entry_of_sexp sexp =
    let open Sexp in
    match sexp with
    | List [ round; apply_id_stamp; trace; action ] ->
      let round = Sexp.int_of_t round in
      let apply_id_stamp = Option.t_of_sexp Sexp.int_of_t apply_id_stamp in
      let trace = list_of_sexp trace_item_of_sexp trace in
      let action = Action.t_of_sexp action in
      { round; apply_id_stamp; trace; action; }
    | _ ->
      raise (Sexp.Parse_error "bla")
  ;;

  let sexp_of_entry entry =
    let open Sexp in
    let { round; apply_id_stamp; trace; action } = entry in
    List [
      Sexp.t_of_int round;
      Option.sexp_of_t Sexp.t_of_int apply_id_stamp;
      Sexp.t_of_list sexp_of_trace_item trace;
      Action.sexp_of_t action;
    ]
  ;;

  let load_from_channel ic =
    Sexp.list_of_t entry_of_sexp (Sexp_file.load_from_channel ic)
  ;;

  let t_of_sexp sexp = Sexp.list_of_t entry_of_sexp sexp

  let sexp_of_t t = Sexp.t_of_list sexp_of_entry t

  let trace_semantically_equal (our_trace : trace_item list) (v1_trace : Trace_item.t list) =
    let rec loop our_trace v1_trace =
      match our_trace, v1_trace with
      | [], [] -> true
      | (our_hd :: our_tl), (v1_hd :: v1_tl) ->
        eq_v1_trace_item our_hd v1_hd && loop our_tl v1_tl
      | _, _ -> false
    in
    loop our_trace v1_trace
  ;;

  let find_decision (overrides : t) ({ V1.Overrides. trace; apply_id; }) =
    match
      let apply_id_stamp = Apply_id.get_stamp apply_id.stamp in
      List.find_opt (fun override ->
          Option.equal (=) apply_id_stamp override.apply_id_stamp
          && trace_semantically_equal override.trace trace)
        overrides
    with
    | None -> None
    | Some a -> Some a.action
  ;;

  let of_v1_overrides (v1_overrides : V1.Overrides.t) =
    List.map (fun (v1_override : V1.Decision.t) ->
        let apply_id_stamp =
          Apply_id.get_stamp v1_override.apply_id.stamp
        in
        let trace =
          List.map (fun (trace_item : V1.Trace_item.t) ->
              match trace_item with
              | V1.Trace_item.At_call_site acs ->
                let linkage_name =
                  Compilation_unit.get_linkage_name (
                    Apply_id.get_compilation_unit acs.apply_id
                  )
                in
                let stamp = Apply_id.get_stamp acs.apply_id.stamp in
                Apply { linkage_name; stamp; }
              | V1.Trace_item.Enter_decl enter_decl ->
                let linkage_name =
                  Compilation_unit.get_linkage_name (
                    Closure_origin.get_compilation_unit (
                      enter_decl.declared.closure_origin
                    )
                  )
                in
                let name =
                  Closure_origin.get_name enter_decl.declared.closure_origin
                in
                Decl { linkage_name; name; })
            v1_override.trace
        in
        { round = v1_override.round;
          apply_id_stamp;
          action = v1_override.action;
          trace;
        })
      v1_overrides
  ;;
end

module Multiversion_overrides = struct
  type t =
    | V0 of V0.t list
    | V1 of V1.Overrides.t
    | V1_tree of V1.Overrides.root option array
    | V_simple of Simple_overrides.t
    | Don't

  let don't = Don't

  type query = V0.Query.t * V1.Overrides.query

  let find_decision t (query : query) =
    match t with
    | Don't -> None

    | V0 overrides ->
      let ({V0.Query. call_stack; applied;}, _) = query in
      begin match V0.find_decision overrides ~call_stack ~applied with
      | Some true  -> Some Action.Inline
      | Some false -> Some Action.Apply
      | None -> None
      end

    | V1 overrides ->
      let (_, query) = query in
      V1.Overrides.find_decision overrides query

    | V_simple overrides ->
      let _, query = query in
      Simple_overrides.find_decision overrides query

    | V1_tree trees ->
      let _, query = query in
      match trees.(query.round) with
      | None -> None
      | Some tree -> V1.Overrides.find_in_tree tree query
  ;;

  let load_from_clflags () =
    match !Clflags.inlining_overrides with
    | None -> Don't
    | Some filename ->
      let ic = open_in filename in
      let sexp = Sexp_file.load_from_channel ic in
      try
        let chosen = Simple_overrides.t_of_sexp sexp in
        let len = List.length chosen in
        Format.printf "Loadded Simple overrides (len = %d) from %s\n"
          len filename;
        V_simple chosen
      with
      | Sexp.Parse_error _  ->
        begin
        try
          let chosen = V1.Overrides.t_of_sexp sexp in
          let len = List.length chosen in
          Format.printf "Loadded V1 overrides (len = %d) from %s\n"
            len filename;
          begin try
            let trees =
              List.init 3 (fun round ->
                  let chosen =
                    List.filter (fun (d : V1.Decision.t) -> d.round = round)
                      chosen
                  in
                  match chosen with
                  | [] -> None
                  | chosen -> Some (V1.Overrides.build_tree chosen))
              |> Array.of_list
            in
            V1_tree trees
          with
          | V1.Overrides.Tree_error ->
            V1 chosen
          end
        with
        | Sexp.Parse_error _  ->
          let res =
            V0 (Sexp.list_of_sexp V0.t_of_sexp sexp)
          in
          Format.printf "Loadded (DEPREACATED) V0 overrides from %s\n"
              filename;
          res
        end
end
