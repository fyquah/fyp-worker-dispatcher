let f _ =
  Data_collector.Action.Inline
;;

let () =
  Inlining_decisions.init_custom_heuristic f
;;
