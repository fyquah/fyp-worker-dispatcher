let rec list_equal equal a b =
  match a, b with
  | ([], []) -> true
  | (_, []) -> false
  | ([], _) -> false
  | (a :: tl_a, b :: tl_b) -> equal a b && list_equal equal tl_a tl_b

let option_equal eq a b =
  match a, b with
  | None, None -> true
  | Some _, None -> false
  | None, Some _ -> false
  | Some a, Some b -> eq a b
