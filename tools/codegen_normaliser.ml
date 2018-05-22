open Common

let (normaliser : Feature_utils.normaliser) =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let ret = Marshal.from_channel ic in
  close_in ic;
  ret
;;

let print_float x =
  if x < 0.0 then
    Format.asprintf "(-.%f)" (abs_float x)
  else
    Format.asprintf "%f" (abs_float x)
;;

let print_decl var_name vars =
  Format.printf "let %s = [\n" var_name;
  List.iter (fun (name, s) ->
      Format.printf "  (\"%s\", %s);\n" name (print_float s))
    vars;
  Format.printf "]\n;;\n\n"
;;

let () =
  print_decl "means" (Feature_list.to_list normaliser.mean);
  print_decl "std" (Feature_list.to_list normaliser.std)
;;
