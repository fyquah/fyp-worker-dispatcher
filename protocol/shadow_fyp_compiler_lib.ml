open Core

let rec comp_sexp_of_core_sexp core_sexp =
  match core_sexp with
  | Core.Sexp.Atom s -> Fyp_compiler_lib.Sexp.Atom s
  | Core.Sexp.List l ->
    Fyp_compiler_lib.Sexp.List (List.map ~f:comp_sexp_of_core_sexp l)
;;

let rec core_sexp_of_comp_sexp comp_sexp =
  match comp_sexp with
  | Fyp_compiler_lib.Sexp.Atom s -> Core.Sexp.Atom s
  | Fyp_compiler_lib.Sexp.List l ->
    Core.Sexp.List (List.map ~f:core_sexp_of_comp_sexp l)
;;

module Make_core_sexp(M :
  sig type t

  val sexp_of_t : t -> Fyp_compiler_lib.Sexp.t
  val t_of_sexp : Fyp_compiler_lib.Sexp.t -> t
end) = struct
  let sexp_of_t t = core_sexp_of_comp_sexp (M.sexp_of_t t)
  let t_of_sexp sexp = M.t_of_sexp (comp_sexp_of_core_sexp sexp)
end

module Compilation_unit = struct
  include Fyp_compiler_lib.Compilation_unit
  include Make_core_sexp(Fyp_compiler_lib.Compilation_unit)
end

module Apply_id = struct
  include Fyp_compiler_lib.Apply_id
  include Make_core_sexp(Fyp_compiler_lib.Apply_id)

  let to_path t =
    match t.parents with
    | None -> [(t.compilation_unit, t.stamp)]
    | Some _ -> get_inlining_path t
  ;;

  let node_id t =
    (t.compilation_unit, t.stamp)
  ;;

  let of_path_inconsistent path =
    let build_directly =
      Fyp_compiler_lib.Apply_id.build_directly
    in
    let call_site_node_id = List.hd_exn (List.rev path) in
    let rev_context_node_ids = List.rev (List.tl_exn (List.rev path)) in
    let parents =
      List.map rev_context_node_ids ~f:(fun node_id ->
          build_directly (fst node_id) (snd node_id) [])
    in
    build_directly (fst call_site_node_id) (snd call_site_node_id) parents
  ;;

  module Stamp = struct
    module T = struct
      type t = stamp

      let sexp_of_t = sexp_of_stamp
      let t_of_sexp = stamp_of_sexp

      let compare a b = compare_stamp a b
    end

    include T
    include Make_core_sexp(T)
  end

  module Node_id = struct
    type t = (Compilation_unit.t * stamp)

    let equal a b =
      Compilation_unit.equal (fst a) (fst b) &&
      Stamp.compare (snd a) (snd b) = 0
    ;;
  end

  module Path = struct
    module T = struct
      type t = (Compilation_unit.t * Stamp.t) list
      [@@deriving sexp, compare]

      let to_string path =
        List.map path ~f:(fun (a, b) ->
            let compilation_unit =
              Fyp_compiler_lib.Linkage_name.to_string (
                Compilation_unit.get_linkage_name a)
            in
            let stamp =
              match b with
              | Fyp_compiler_lib.Apply_id.Plain_apply x ->
                sprintf "Apply[%d]" x
              | Fyp_compiler_lib.Apply_id.Over_application x ->
                sprintf "Over[%d]" x
              | Fyp_compiler_lib.Apply_id.Stub -> "Stub"
            in
            sprintf "%s__%s" compilation_unit stamp)
        |> String.concat ~sep:"/"
        |> (fun s -> "/" ^ s)
      ;;

      let to_readable_string a =
        to_string (
          Option.value ~default:[]
            (Option.map ~f:(fun x -> [x]) (List.hd (List.rev a))))
      ;;
    end

    include T
    include Comparable.Make(T)
  end

  let compare a b = Path.compare (to_path a) (to_path b)
end

module Closure_id = struct
  include Fyp_compiler_lib.Closure_id
  include Make_core_sexp(Fyp_compiler_lib.Closure_id)
end

module Call_site = struct
  include Fyp_compiler_lib.Call_site
  include Make_core_sexp(Fyp_compiler_lib.Call_site)
end

module Closure_origin = struct
  include Fyp_compiler_lib.Closure_origin
  include Make_core_sexp(Fyp_compiler_lib.Closure_origin)
end

module Data_collector = struct
  module V0 = struct
    include Fyp_compiler_lib.Data_collector.V0
    include Make_core_sexp(Fyp_compiler_lib.Data_collector.V0)

    (* TODO(fyq14): This is bad for perf! *)
    let compare (a : t) (b : t) =
      Sexp.compare (sexp_of_t a) (sexp_of_t b)
    ;;
  end

  module V1 = struct
    module Function_metadata = struct
      include Fyp_compiler_lib.Data_collector.V1.Function_metadata
      include Make_core_sexp(Fyp_compiler_lib.Data_collector.V1.Function_metadata)

      let pprint (t : t) =
        let closure_origin =
          Format.asprintf "%a" Closure_origin.print t.closure_origin
        in
        let closure_id =
          match t.closure_id with
          | None -> "NONE"
          | Some closure_id -> Format.asprintf "%a" Closure_id.print closure_id
        in
        sprintf "closure_origin = %s | closure_id = %s"
          closure_origin closure_id
      ;;

      let equal a b = (compare a b = 0)
    end

    module Trace_item = struct
      include Fyp_compiler_lib.Data_collector.V1.Trace_item
      include Make_core_sexp(Fyp_compiler_lib.Data_collector.V1.Trace_item)
    end

    module Action = struct
      include Fyp_compiler_lib.Data_collector.V1.Action
      include Make_core_sexp(Fyp_compiler_lib.Data_collector.V1.Action)
    end

    module Decision = struct
      include Fyp_compiler_lib.Data_collector.V1.Decision
      include Make_core_sexp(Fyp_compiler_lib.Data_collector.V1.Decision)
    end

    module Overrides = struct
      include Fyp_compiler_lib.Data_collector.V1.Overrides
      include Make_core_sexp(Fyp_compiler_lib.Data_collector.V1.Overrides)
    end
  end

  module Simple_overrides = struct
    include Fyp_compiler_lib.Data_collector.Simple_overrides
    include Make_core_sexp(Fyp_compiler_lib.Data_collector.Simple_overrides)
  end

  include V1
end

module Call_site_offset = struct
  include Call_site.Offset
  include Make_core_sexp(Call_site.Offset)

  let compare a b = Int.compare (to_int a) (to_int b)
end
