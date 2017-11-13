[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async
open Common

module SA = Optimization.Simulated_annealing.Make(struct
    module T = struct
      type state = int list [@@deriving sexp]

      type t = state [@@deriving sexp]
      
      let compare a b = List.compare Int.compare a b
    end

    include T
    include Comparable.Make(T)
  end)

let command =
  let open Command.Let_syntax in
  Command.async ~doc:"Command"
    Command.Param.return (fun () -> Deferred.unit)
