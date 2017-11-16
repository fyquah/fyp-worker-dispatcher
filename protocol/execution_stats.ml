open Core

module Gc_stats = struct
  type t =
    { major_collections : int;
      minor_collections : int;
      compactions       : int;

      minor_words       : int;
      promoted_words    : int;
      major_words       : int;

      top_heap_words    : int;
      heap_words        : int;
      live_words        : int;
      free_words        : int;
      largest_free      : int;
      fragments         : int;

      live_blocks       : int;
      free_blocks       : int;
      heap_chunks       : int;
    }
  [@@deriving sexp, fields, bin_io]

  let parse lines =
    let re = Re2.Regex.create_exn "\\s+" in
    try
      let map =
        List.fold lines ~init:String.Map.empty ~f:(fun acc line ->
          let line = Re2.Regex.replace_exn ~f:(fun _ -> "") re line in
          match String.split line ~on:':' with
          | key :: data :: [] ->
            String.Map.add acc ~key ~data:(Int.of_string data)
          | _ -> acc)
      in
      let g k = String.Map.find_exn map k in
      Some {
        major_collections = g "major_collections";
        minor_collections = g "minor_collections";
        compactions = g "compactions";

        minor_words = g "minor_words";
        promoted_words = g "promoted_words";
        major_words = g "major_words";

        top_heap_words = g "top_heap_words";
        heap_words     = g "heap_words";
        live_words     = g "live_words";
        free_words     = g "free_words";
        largest_free   = g "largest_free";
        fragments      = g "fragments";

        live_blocks = g "live_blocks";
        free_blocks = g "free_blocks";
        heap_chunks = g "heap_chunks";
      }
    with Not_found -> None
end

type t =
  { raw_execution_time : Time.Span.t list;
    worker_hostname    : string sexp_option;
    gc_stats           : string;
    parsed_gc_stats    : Gc_stats.t sexp_option
  }
[@@deriving bin_io, sexp]
