type 'a t =
  | Leaf of 'a
  | Node of ('a t * 'a t)

let rec f x =
  match x with
  | Leaf a -> Format.print_int a
  | Node (left, right) -> f left; f right
;;

let () =
  let tree =
    Node (
      Leaf 1,
      Leaf 1
    )
  in
  f tree
;;
