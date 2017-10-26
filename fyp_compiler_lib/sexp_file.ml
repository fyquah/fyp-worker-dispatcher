[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Sexp

exception Empty_file

let load_from_channel ic =
  let lexbuf = Lexing.from_channel ic in
  match Sexp_parser.prog Sexp_lexer.read lexbuf with
  | None ->
    close_in ic;
    raise Empty_file
  | Some sexp ->
    close_in ic;
    sexp
;;
