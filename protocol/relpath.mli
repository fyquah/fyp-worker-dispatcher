open Core

type t [@@deriving sexp, bin_io, compare]
include Stringable with type t := t
