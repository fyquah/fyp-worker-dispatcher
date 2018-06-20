let () =
  Printf.eprintf "Compilation flags:\n";
  Printf.eprintf "- unbox_closures = %b\n" (!Clflags.unbox_closures);

  for i = 0 to 2 do
    Printf.eprintf "- inline_max_unroll[%d] = %d\n" i
      (Clflags.Int_arg_helper.get ~key:i !Clflags.inline_max_unroll)
  done;

  for i = 0 to 2 do
    Printf.eprintf "- inline_max_depth[%d] = %d\n" i
      (Clflags.Int_arg_helper.get ~key:i !Clflags.inline_max_depth)
  done;

  for i = 0 to 2 do
    Printf.eprintf "- inline_threshold[%d] = %.3f\n" i
        (Clflags.Float_arg_helper.get ~key:i !Clflags.inline_threshold)
  done
;;
