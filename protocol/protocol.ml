open Core
open Async

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

module Job_dispatch_rpc = struct
  module Query = struct
    type t =
      { compile_params : Compile_params.t option;
      }
    [@@deriving bin_io, sexp]
  end

  module Response = struct
    type t =
      { execution_time : Time.Span.t;
      }
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
    }
  [@@deriving sexp]
end
