let rec factorial n =
  if n <= 1 then
    1
  else
    n * factorial (n - 1)
;;

let () =
  Format.printf "%d\n" (List.length []);
  Format.printf "Hello world %d" (factorial (int_of_string (Sys.argv.(1))))
