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
      }

    let sexp_of_t t =
      Sexp.List [
        Option.sexp_of_t Closure_id.sexp_of_t t.closure_id;
        Option.sexp_of_t Set_of_closures_id.sexp_of_t None;
        Closure_origin.sexp_of_t t.closure_origin;
      ]
    ;;

    let t_of_sexp sexp =
      let open Sexp in
      match sexp with
      | List [closure_id; _set_of_closures_id; closure_origin] ->
        let closure_id =
          Option.t_of_sexp Closure_id.t_of_sexp closure_id
        in
        let set_of_closures_id = None in  (* TODO(fyq14): Actually import this *)
        let closure_origin = Closure_origin.t_of_sexp closure_origin in
        { closure_id; set_of_closures_id; closure_origin; }
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

    let semantically_equal a b =
      match a, b with
      | Enter_decl a, Enter_decl b ->
        Closure_origin.equal a.declared.closure_origin b.declared.closure_origin

      | At_call_site a, At_call_site b ->
        Apply_id.equal a.apply_id a.apply_id &&
          (Closure_origin.equal
            a.applied.closure_origin b.applied.closure_origin)

      | _ -> false
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
      | Apply  (* Aka do nothing *)

    let sexp_of_t = function
      | Inline -> Sexp.Atom "Inline"
      | Apply -> Sexp.Atom "Apply"

    let t_of_sexp = function
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
      trace: Trace_item.t list;
      apply_id: Apply_id.t;
    }

    let find_decision overrides ({ trace; apply_id; }) =
      match
        List.find_opt (fun (decision : Decision.t) ->
            Apply_id.equal decision.apply_id apply_id
            && Helper.list_equal Trace_item.semantically_equal decision.trace trace)
          overrides
      with
      | None -> None
      | Some a -> Some a.Decision.action
    ;;

    let sexp_of_t t =
      Sexp.List (List.map Decision.sexp_of_t t)
    ;;

    let t_of_sexp = function
      | Sexp.List decisions -> List.map Decision.t_of_sexp decisions
      | _ -> raise (Sexp.Parse_error "oops")
    ;;

    let load_from_channel ic = t_of_sexp (Sexp_file.load_from_channel ic)
  end
end

include V1

module Multiversion_overrides = struct
  type t =
    | V0 of V0.t list
    | V1 of V1.Overrides.t
    | Don't

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

  let load_from_clflags () =
    match !Clflags.inlining_overrides with
    | None -> Don't
    | Some arg ->
      match String.split_on_char ':' arg with
      | [ "v0" ; filename ] ->
        let ic = open_in filename in
        let ts = V0.load_from_channel ic in
        V0 ts
      | [ "v1" ; filename ]
      | [ filename ] -> 
        let ic = open_in filename in
        let ts = V1.Overrides.load_from_channel ic in
        V1 ts
      | _ ->
        Misc.fatal_errorf "Failed to parse -inlining-overrides flag %s" arg
end
