[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t =
  { call_stack : Call_site.t list;
    applied : Closure_id.t;
    decision : bool;
  }

let (inlining_decisions : t list ref) = ref []

let (inlining_overrides : t list ref) = ref []

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
  let out_channel = open_out (output_prefix ^ ".data_collector.sexp") in
  let ppf = Format.formatter_of_out_channel out_channel in
  let sexp = Sexp.sexp_of_list sexp_of_t !inlining_decisions in
  Format.fprintf ppf "%a" Sexp.print_mach sexp;
  close_out out_channel;
  inlining_decisions := []
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

let find_decision ~call_stack ~applied =
  match
    List.find_opt (fun a ->
        Closure_id.equal a.applied applied
        && Helper.list_equal Call_site.equal a.call_stack call_stack)
      !inlining_overrides
  with
  | None -> None
  | Some a -> Some a.decision
;;
