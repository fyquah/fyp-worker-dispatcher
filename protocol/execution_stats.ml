open Core

type t =
  { raw_execution_time : Time.Span.t list;
  }
[@@deriving bin_io, sexp]
