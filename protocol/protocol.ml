open Core
open Async

module Relpath = Relpath
module Results = Results
module Execution_stats = Execution_stats
module Inlining_tree = Inlining_tree
module Shadow_fyp_compiler_lib = Shadow_fyp_compiler_lib

module Absolute_path = struct
  open Shadow_fyp_compiler_lib

  module T = struct
    type item =
      | Apply of Apply_id.Path.t
      | Decl  of Closure_origin.t
    [@@deriving sexp, compare]

    type t = item list [@@deriving sexp, compare]

    let of_trace (trace : Feature_extractor.trace_item list) =
      List.map trace ~f:(fun item ->
        match item with
        | Feature_extractor.Apply app ->
          Apply (Apply_id.to_path app)
        | Feature_extractor.Decl  co ->
          Decl co)
    ;;

    let compress path =
      let last_seen : [ `Decl | `Apply ] ref  = ref `Decl in
      List.filter_map (List.rev path) ~f:(fun p ->
          match p with
          | Decl  _ -> 
            last_seen := `Decl;
            Some p
          | Apply _ -> 
            match !last_seen with
            | `Apply -> None
            | `Decl -> 
              last_seen := `Apply;
              Some p)
      |> List.rev
    ;;
  end

  include T
  include Comparable.Make(T)
end

module Compile_params = struct
  type t =
    { inline                 : float;  (* 10.0 *)
      inline_top_level       : int;
      inline_alloc_cost      : int;
      inline_branch_cost     : int;
      inline_call_cost       : int;
      inline_prim_cost       : int;
      inline_indirect_cost   : int;
      inline_lifting_benefit : int;
    }
  [@@deriving sexp, bin_io]
end

module Benchmark = struct
  type t =
    { dir        : Relpath.t; (* Path to benchmark executable, relative to
                                 [rundir] *)
      executable : string;
      run_args   : string list;
    }
  [@@deriving bin_io, sexp]
end

module Compiler_selection = struct
  type t =
    | Flambda
    | Ours
  [@@deriving bin_io, sexp]

end

module Info_rpc = struct
  module Query = struct
    type t = Where_to_copy [@@deriving sexp, bin_io]
  end

  module Response = struct
    type t =
      { rundir  : string;
        relpath : Relpath.t;
      }
    [@@deriving sexp, bin_io]
  end

  let rpc =
    Rpc.Rpc.create ~name:"information" ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end


module Job_dispatch_rpc = struct
  module Query = struct
    type t =
      | Run_binary of Relpath.t
    [@@deriving bin_io, sexp]
  end

  module Response = struct
    (* I think it is not good to use [Or_error.t] here as it may raise
     * confusion in the rpc interface (imagine running [Or_error.t] being
     * chained up to three times...)
     *)
    type t =
      | Success of Execution_stats.t
      | Failed of Error.t
    [@@deriving bin_io, sexp]
  end

  let rpc =
    Rpc.Rpc.create ~name:"dispatcher" ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Config = struct
  module Rpc_worker_config = struct
    type t =
      { port : int;
        worker_direct_exec_dir: Relpath.t;
        hostname : string;
      }
    [@@deriving sexp]
  end

  module Ssh_worker_config = struct
    type t =
      { rundir    : string; (* We will setup overselves *)
        hostname  : string;
        user      : string;
        processor : int sexp_option;
     }
    [@@deriving sexp]
  end

  type worker_config =
    | Rpc_worker of Rpc_worker_config.t
    | Ssh_worker of Ssh_worker_config.t
  [@@deriving sexp]

  let hostname worker_config =
    match worker_config with
    | Rpc_worker r -> r.hostname
    | Ssh_worker s -> s.hostname

  type t =
    { num_runs: int;
      worker_configs: worker_config list;
    }
  [@@deriving sexp]
end
