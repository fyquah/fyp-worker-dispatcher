[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type offset = int

type at_call_site =
  { source    : Closure_id.t option;
    offset    : offset;
    applied   : Closure_id.t;
  }

type enter_decl =
  { source     : Closure_id.t option;
    closure    : Closure_id.t;
  }

type t =
  | Enter_decl of enter_decl
  | At_call_site of at_call_site

let at_call_site_equal (a : at_call_site) (b : at_call_site)  =
  a.offset = b.offset &&
  Helper.option_equal Closure_id.equal a.source b.source &&
  Closure_id.equal a.applied b.applied

let enter_decl_equal (a : enter_decl) (b : enter_decl) =
  Closure_id.equal a.closure b.closure &&
  Helper.option_equal Closure_id.equal a.source b.source
;;

let equal a b =
  match a, b with
  | Enter_decl a, Enter_decl b -> enter_decl_equal a b
  | At_call_site a, At_call_site b -> at_call_site_equal a b
  | _ , _ -> false

let base_offset = 0

let inc offset = offset + 1

let enter_decl ~source ~closure = Enter_decl { source; closure }

let create_top_level applied offset =
  At_call_site { source = None; offset ; applied }
;;

let create ~source ~applied offset =
  At_call_site { source = Some source; offset; applied }
;;

let closure_id_of_sexp sexp =
  Closure_id.wrap (Variable.of_sexp sexp)
;;

let closure_id_to_sexp closure_id =
  Variable.to_sexp (Closure_id.unwrap closure_id)
;;

let offset_to_sexp o = Sexp.Atom (string_of_int o)

let option_closure_id_to_sexp s =
  match s with
  | None -> Sexp.Atom "TOP_LEVEL"
  | Some c -> closure_id_to_sexp c

let option_closure_id_of_sexp sexp =
  match sexp with
  | Sexp.Atom "TOP_LEVEL" -> None
  | otherwise -> Some (closure_id_of_sexp otherwise)

let offset_of_sexp sexp =
  match sexp with
  | Sexp.Atom s -> int_of_string s
  | _ ->
    Misc.fatal_errorf "Cannot parse %a as an offset"
      Sexp.print_mach sexp

let to_sexp sexp =
  let open Sexp in
  match sexp with
  | Enter_decl enter_decl ->
    List [
      Atom "Enter_decl";
      option_closure_id_to_sexp enter_decl.source;
      closure_id_to_sexp enter_decl.closure;
    ]
  | At_call_site { source ; offset; applied } ->
    List [
      Atom "At_call_site";
      option_closure_id_to_sexp source;
      offset_to_sexp offset;
      closure_id_to_sexp applied
    ]

let of_sexp sexp =
  let open Sexp in
  match sexp with
  | List (Atom "Enter_decl" :: source :: closure :: []) ->
    let source = option_closure_id_of_sexp source in
    let closure = closure_id_of_sexp closure in
    Enter_decl { source; closure }
  | List (Atom "At_call_site" :: source :: offset :: applied :: []) ->
    let source = option_closure_id_of_sexp source in
    let offset = offset_of_sexp offset in
    let applied = closure_id_of_sexp applied in
    At_call_site { source ; offset; applied }
  | _ ->
    Misc.fatal_errorf "Cannot parse %a as a Call_site.t"
      Sexp.print_mach sexp

let pprint_source ppf t =
  match t with
  | None -> Format.fprintf ppf "TOP_LEVEL"
  | Some c -> Format.fprintf ppf "%a" Closure_id.print c

let pprint ppf t =
  match t with
  | At_call_site at_call_site ->
    Format.fprintf ppf "%a<%a:%d>"
      Closure_id.print at_call_site.applied
      pprint_source at_call_site.source
      at_call_site.offset;

  | Enter_decl enter_decl ->
    Format.fprintf ppf "{%a}<%a>"
      Closure_id.print enter_decl.closure
      pprint_source enter_decl.source
