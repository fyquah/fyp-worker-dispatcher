type 'a t =
  | Mat of 'a array array
  | Vec of 'a array
  | Scalar of 'a


let of_float x = Scalar x
let of_int   x = Scalar x

let map f v =
  match v with
  | Mat mat ->
    Mat (Array.map (fun vec -> Array.map (fun a -> f a) vec) mat)
  | Vec v -> Vec (Array.map (fun a -> f a) v)
  | Scalar a -> Scalar (f a)
;;

let bool_not t = map (fun a -> not a) t

let eval_bool t =
  | Scalar b ->  b
  | _ -> false (** suffices for now **)
;;

let shape = function
  | Mat a    -> [| Array.length a; Array.length (a.(0)); |]
  | Vec a    -> [| Array.length a |]
  | Scalar _ -> [| |]
;;

let notequal a b =
  not (a = b)  (* polymorphic compare @_@ *)
;;

let map2 (type a) f (t_a : a t) (t_b : a t) = 
  match t_a, t_b with
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

let relu a = map (fun x -> if x > 0.0 then x else 0.0) a

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
;;


let matmul ta tb =
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

  | _, _ -> assert false
;;


(* TODO *)
let get_variable (_name : string) (_shape : int array) =
  Scalar 1.0
;;

let merge a b = function
  | Some _, Some _
  | None, None -> assert False
  | Some a, None
  | Noe, None -> a
