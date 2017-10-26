[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type offset = int

type at_call_site =
  { closure_id : Closure_id.t option;
    offset     : offset;
  }

type t =
  | Enter_decl of Closure_id.t
  | At_call_site of at_call_site

let at_call_site_equal a b =
  a.offset = b.offset &&
  Helper.option_equal Closure_id.equal a.closure_id b.closure_id

let equal a b =
  match a, b with
  | Enter_decl a, Enter_decl b -> Closure_id.equal a b
  | At_call_site a, At_call_site b -> at_call_site_equal a b
  | _ , _ -> false

let base_offset = 0

let inc offset = offset + 1

let enter_decl closure_id = Enter_decl closure_id

let create_top_level offset =
  At_call_site { closure_id = None; offset }
;;

let create closure_id offset =
  At_call_site { closure_id = Some closure_id; offset; }
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
  | Enter_decl closure_id ->
    List [ Atom "Enter_decl"; closure_id_to_sexp closure_id; ]
  | At_call_site { closure_id ; offset } ->
    List [
      Atom "At_call_site";
      option_closure_id_to_sexp closure_id;
      offset_to_sexp offset;
    ]

let of_sexp sexp =
  let open Sexp in
  match sexp with
  | List (Atom "Enter_decl" :: closure_id :: []) ->
    Enter_decl (closure_id_of_sexp closure_id)
  | List (Atom "At_call_site" :: closure_id :: offset :: []) ->
    let closure_id = option_closure_id_of_sexp closure_id in
    let offset = offset_of_sexp offset in
    At_call_site { closure_id ; offset;  }
  | _ ->
    Misc.fatal_errorf "Cannot parse %a as a Call_site.t"
      Sexp.print_mach sexp

let pprint ppf t =
  match t with
  | At_call_site at_call_site ->
    begin match at_call_site.closure_id with
    | None -> Format.fprintf ppf "TOP_LEVEL"
    | Some c -> Format.fprintf ppf "%a" Closure_id.print c
    end;
    Format.fprintf ppf ":%d" at_call_site.offset;
  | Enter_decl closure_id ->
    Format.fprintf ppf "{%a}" Closure_id.print closure_id
