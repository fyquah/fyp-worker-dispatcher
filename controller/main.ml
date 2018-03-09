[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core

let () =
  Command.group ~summary:"Controller code"
    [ ("t-test-search", Command_t_test_search.command);
      ("simulated-annealing", Command_simulated_annealing.command);
      ("rl", Command_rl.command);
      ("random", Command_random.command);
    ]
  |> Command.run
