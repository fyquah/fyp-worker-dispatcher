type t =
  | Atom of string
  | List of t list

exception Parse_error of string

let rec print_mach ppf t =
  match t with
  | Atom s -> Format.fprintf ppf "%s" s
  | List ts ->
    Format.fprintf ppf "(%a)"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
        (fun ppf t -> Format.fprintf ppf "%a" print_mach t))
      ts

let t_of_bool b =
  match b with
  | true -> Atom "true"
  | false -> Atom "false"

let bool_of_t t =
  match t with
  | Atom "true" -> true
  | Atom "false" -> false
  | otherwise ->
    let msg =
      Format.asprintf "Cannot parse %a as a bool" print_mach otherwise
    in
    raise (Parse_error msg)

let sexp_of_list sexp_of_a l = List (List.map sexp_of_a l)
let t_of_list = sexp_of_list

let list_of_sexp a_of_sexp sexp =
  match sexp with
  | List l -> List.map a_of_sexp l
  | otherwise ->
    raise (Parse_error (
      Format.asprintf "Cannot parse %a as list" print_mach otherwise))

let list_of_t = list_of_sexp
