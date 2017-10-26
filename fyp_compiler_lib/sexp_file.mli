[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include module type of Sexp

val load_from_channel : in_channel -> Sexp.t
