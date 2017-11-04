open Core

type t =
  { raw_execution_time : Time.Span.t list;
    worker_hostname    : string sexp_option;
  }
[@@deriving bin_io, sexp]
