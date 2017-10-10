open Core
open Async

module Relpath = Relpath

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

module Benchmark_results = struct
  type t =
    { execution_time : Time.Span.t;
    }
  [@@deriving bin_io, sexp]
end

module Job_dispatch_rpc = struct
  module Query = struct
    type t =
      { compile_params : Compile_params.t option;
        targets        : Benchmark.t;
      }
    [@@deriving bin_io, sexp]
  end

  module Response = struct
    (* I think it is not good to use [Or_error.t] here as it may raise
     * confusion in the rpc interface (imagine running [Or_error.t] being
     * chained up to three times...)
     *)
    type t =
      | Success of Benchmark_results.t
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
  type t =
    { worker_port : int;
      num_runs: int;
    }
  [@@deriving sexp]
end
