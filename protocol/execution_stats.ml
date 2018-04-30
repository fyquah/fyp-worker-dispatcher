open Core
open Async

module Perf_stats = struct
  type entry =
    { name          : string;
      value         : int;
      measured_ns   : int;
      deviation     : float sexp_option;
      scaled_from   : float;
      comment       : string sexp_option;
    }
  [@@deriving sexp, bin_io]

  type t = entry list [@@deriving sexp, bin_io]

  let to_map (t : t) =
    List.fold t ~init:String.Map.empty ~f:(fun unchanged entry ->
      String.Map.update unchanged entry.name ~f:(fun _ -> entry))
  ;;

  exception Parse_error

  let parse lines =
    String.split_lines lines
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.filter_map ~f:(fun line ->
        try
          let line = String.split ~on:',' line |> Array.of_list in
          let i = ref 0 in
          let value = line.(!i) in
          if String.equal value "<not supported>" then begin
            raise Parse_error
          end;
          let value = Int.of_string value in
          i := !i + 2;
          let name = line.(!i) in
          i := !i + 1;
          let deviation =
            let len = String.length line.(!i) in
            let last_char = String.get line.(!i) (len - 1) in
            if Char.equal '%' last_char then begin
              i := !i + 1;
              Some (Float.of_string (String.drop_suffix line.(!i) 1) /. 100.0)
            end else
              None
          in
          let measured_ns = Int.of_string line.(!i) in
          i := !i + 1;
          let scaled_from = Float.of_string line.(!i) /. 100.0 in
          i := !i + 1;
          let comment =
            match line.(!i) with
            | "" -> None
            | x -> Some x
          in
          Some { name; value; measured_ns; deviation; scaled_from; comment }
        with
        | Parse_error -> None
        | Failure _ -> None)
  ;;
end

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
            String.Map.update acc key ~f:(fun _ -> Int.of_string data)
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
    parsed_gc_stats    : Gc_stats.t sexp_option;
    perf_stats         : Perf_stats.t list sexp_option;
  }
[@@deriving bin_io, sexp]
