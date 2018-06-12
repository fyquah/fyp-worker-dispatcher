type 'a t =
  | Mat of 'a array array
  | Vec of 'a array
  | Scalar of 'a
  | Nothing


let of_float x = Scalar x
let of_int   x = Scalar x

let map f v =
  match v with
  | Mat mat ->
    Mat (Array.map (fun vec -> Array.map (fun a -> f a) vec) mat)
  | Vec v -> Vec (Array.map (fun a -> f a) v)
  | Scalar a -> Scalar (f a)
  | Nothing -> Nothing
;;

let bool_not t = map (fun a -> not a) t

let eval_bool t =
  match t with
  | Nothing -> false
  | Scalar b -> b
  | _ -> false (** suffices for now **)
;;

let shape = function
  | Mat a    -> [| Array.length a; Array.length (a.(0)); |]
  | Vec a    -> [| Array.length a |]
  | Scalar _ -> [| |]
  | Nothing  -> [| |]
;;

let notequal a b =
  Scalar (not (a = b))  (* polymorphic compare @_@ *)
;;

let rec approx_equal a b =
  try
    let eq a b = abs_float (a -. b) < 0.00001 in
    match a, b with
    | Scalar a, Scalar b -> eq a b
    | Vec a, Vec b ->
      Array.map2 (fun a b -> (a, b)) a b
      |> Array.for_all (fun (a, b) -> eq a b)
    | Mat a, Mat b ->
      Array.map2 (fun a b -> (a, b)) a b
      |> Array.for_all (fun (a, b) ->
          approx_equal (Vec a) (Vec b))
    | _, _ -> false
  with
  | _ -> false

let map2 (type a) f (t_a : a t) (t_b : a t) = 
  match t_a, t_b with
  | _ , Nothing -> assert false
  | Nothing , _ -> assert false

  | Mat m_a, Mat m_b ->
    Mat (Array.map2 (Array.map2 f) m_a m_b)
  | Mat m_a, Vec v_b ->
    Mat (Array.map (fun v_a -> Array.map2 f v_a v_b) m_a)
  | Mat m_a, Scalar b ->
    Mat (Array.map (Array.map (fun a -> f a b)) m_a)

  | Vec v_a, Mat m_b ->
    Mat (Array.map (fun v_b -> Array.map2 f v_a v_b) m_b)
  | Vec v_a, Vec v_b ->
    Vec (Array.map2 f v_a v_b)
  | Vec v_a, Scalar b ->
    Vec (Array.map (fun a -> f a b) v_a)

  | Scalar a, Mat m_b ->
    Mat (Array.map (Array.map (fun b -> f a b)) m_b)
  | Scalar a, Vec v_b ->
    Vec (Array.map (fun b -> f a b) v_b)

  | Scalar a, Scalar b -> Scalar (f a b)
;;

let randomuniform = function
  | [| |] -> Scalar (Random.float 1.0)
  | [| len |] ->
    Vec (Array.init len (fun _ -> Random.float 1.0))
  | [| rows; cols |] ->
    Mat (
      Array.init rows (fun _ ->
          Array.init cols (fun _ -> Random.float 1.0))
    )
  | _ -> assert false
;;

let add a b = map2 (+.) a b

let mul a b = map2 ( *. ) a b

let floor a = map floor a

let relu a = map (fun x -> if x > 0.0 then x else 0.0) a

let logistic a = map (fun x -> 1.0 /. (1.0 +. 2.71818284 ** (-.x))) a

let reciprocal a = map (fun x -> 1.0 /. x) a

let softmax t =
  let softmax v =
    let v = Array.map (fun x -> 2.7182818284 ** x) v in
    let sum = Array.fold_left (+.) 0.0 v in
    Array.map (fun x -> x /. sum) v
  in
  match t with
  | Scalar _ -> assert false
  | Mat m_a -> Mat (Array.map (fun v_a -> softmax v_a) m_a)
  | Vec v_a -> Vec (softmax v_a)
  | Nothing -> Nothing
;;


let rec matmul ta tb =
  match ta, tb with
  | Mat m_a, Mat m_b ->
    let mat_shape m =
      match shape (Mat m) with
      | [| r; c; |] -> (r, c)
      | _ -> assert false
    in
    let (rows_a, cols_a) = mat_shape m_a in
    let (rows_b, cols_b) = mat_shape m_b in
    let ret =
      Array.init rows_a (fun _ -> Array.init cols_b (fun _ -> 0.0))
    in
    assert (cols_a = rows_b);
    for i = 0 to rows_a - 1 do
      for j = 0 to cols_b - 1 do
        for k = 0 to cols_a - 1 do
          ret.(i).(j) <-
            ret.(i).(j) +. m_a.(i).(k) *. m_b.(k).(j)
        done
      done
    done;
    Mat ret
  | Vec v_a, Mat m_b ->
    begin match matmul (Mat [| v_a; |]) (Mat m_b) with
    | Mat ret -> Vec ret.(0)
    | _ -> assert false
    end
  | Vec v_a, Vec v_b ->
    Scalar ((Array.map2 ( *. ) v_a v_b) |> Array.fold_left (+.) 0.0)

  | _, _ -> assert false
;;


(* TODO *)
let get_variable (_name : string) (_shape : int array) =
  Scalar 1.0
;;

let merge a b =
  match a, b with
  | Nothing, Nothing -> assert false
  | Nothing, b -> b
  | a, Nothing -> a
  | _, _ -> assert false
;;

module Feature_list = Feature_utils.Feature_list

let unpack_scalar_exn = function
  | Scalar a -> a
  | Vec v ->
    if Array.length v = 1 then
      v.(0)
    else
     assert false
  | _ -> assert false
;;

let features_to_t
    ~(int_features: int Feature_list.t)
    ~numeric_features ~bool_features
    ~numeric_features_indices ~bool_features_indices
    ~numeric_features_means ~numeric_features_std =
  let to_array x =
    Feature_list.to_list x |> List.map (fun (_, a) -> a ) |> Array.of_list
  in
  let bool_to_float x = if x then 1.0 else 0.0 in
  let numeric_features =
    let arr = to_array numeric_features in
    Array.mapi (fun i j ->
        (arr.(j) -. numeric_features_means.(i)) /. numeric_features_std.(i))
      numeric_features_indices
  in
  let bool_features =
    let arr = to_array bool_features in
    Array.map (fun j -> arr.(j)) bool_features_indices
    |> Array.map bool_to_float
  in
  let features =
    Vec (Array.concat [ numeric_features; bool_features; ])
  in
  features
;;

let sigmoid x = 1.0 /. (1.0 +. (2.7182818284 ** (-.x)))

let check_names ~names feature_list  =
  let a =
    Feature_list.to_list feature_list
    |> List.map fst
    |> Array.of_list
  in
  Array.iter2 (fun a b -> assert (String.equal a b)) a names
;;

let choose_cluster ~means features =
  let best = ref 0 in
  let lowest_distance = ref infinity in

  Array.iteri (fun i cluster_mean ->
      let dist =
        Array.map2 (fun a b -> (a -. b) ** 2.0) cluster_mean features
        |> Array.fold_left (+.) 0.0
      in
      if dist < !lowest_distance then begin
        lowest_distance := dist;
        best := i
      end)
    means;

  !best
;;

let choose_cluster ~means features =
  match features with
  | Vec features -> choose_cluster ~means features
  | _ -> assert false
;;

let argmax (arr : float array) =
  let rec loop ~acc i =
    if i >= Array.length arr then
      acc
    else if arr.(i) > arr.(acc) then
      loop ~acc:i (i + 1)
    else
      loop ~acc (i + 1)
  in
  loop ~acc:0 1
;;
