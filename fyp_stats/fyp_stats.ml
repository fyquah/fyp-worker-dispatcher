open Core

let geometric_mean l =
  let power = 1.0 /. float_of_int (List.length l) in
  let base = List.fold l ~init:1.0 ~f:(fun a b -> a *. b) in
  base ** power
;;
