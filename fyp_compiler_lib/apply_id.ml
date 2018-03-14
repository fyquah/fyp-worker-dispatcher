type label = [ `Plain_apply | `Over_application | `Stub ]

type stamp =
  | Plain_apply of int
  | Over_application of int
  | Stub

let sexp_of_stamp stamp = 
  let open Sexp in
  match stamp with
  | Plain_apply x ->
    List [ Atom "Plain_apply"; Atom (string_of_int x) ]
  | Over_application x ->
    List [ Atom "Over_application"; Atom (string_of_int x) ]
  | Stub ->
    List [ Atom "Stub" ]
;;

let stamp_of_sexp stamp =
  let open Sexp in
  match stamp with
  | List [ Atom "Plain_apply"; Atom x ] -> Plain_apply (int_of_string x)
  | List [ Atom "Over_application"; Atom x ] -> Over_application (int_of_string x)
  | List [ Atom "Stub" ] -> Stub
  | _ -> raise (Sexp.Parse_error "oops")
;;

type t = {
    compilation_unit : Compilation_unit.t;
    stamp            : stamp;
    parents          : t list option;
  }

let rec sexp_of_t t =
  match t.parents with
  | None -> 
    Sexp.List [
      Compilation_unit.to_sexp t.compilation_unit;
      sexp_of_stamp t.stamp;
    ]
  | Some x ->
    let path_sexp = Sexp.t_of_list sexp_of_t x in
    Sexp.List [
      Compilation_unit.to_sexp t.compilation_unit;
      sexp_of_stamp t.stamp;
      path_sexp;
    ]
;;

let rec t_of_sexp = function
  | Sexp.List [ a; b] ->
    let compilation_unit = Compilation_unit.of_sexp a in
    let stamp = stamp_of_sexp b in
    let parents = None in
    { compilation_unit; stamp; parents; }
  | Sexp.List [ a; b; c; ] ->
    let compilation_unit = Compilation_unit.of_sexp a in
    let stamp = stamp_of_sexp b in
    let parents = Some (Sexp.list_of_t t_of_sexp c) in
    { compilation_unit; stamp; parents; }
  | _ -> raise (Sexp.Parse_error "oops")
;;


let get_stamp_exn = function
  | Plain_apply a
  | Over_application a -> a
  | Stub -> assert false

let get_stamp = function
  | Plain_apply a
  | Over_application a -> Some a
  | Stub -> None

let compare_stamp t1 t2 = 
  match t1, t2 with
  | Stub, Stub -> 0
  | Stub, _ -> 1
  | _ , Stub -> 0
  | _, _ -> get_stamp_exn t1 - get_stamp_exn t2

let string_of_stamp = function
  | Plain_apply a -> "(Plain_apply " ^ string_of_int a ^ ")"
  | Over_application a -> "(Over_application " ^ string_of_int a ^ ")"
  | Stub -> "(Stub)"

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = compare_stamp t1.stamp t2.stamp in
      if c <> 0 then c
      else Compilation_unit.compare t1.compilation_unit t2.compilation_unit

  let equal t1 t2 =
    if t1 == t2 then true
    else
      t1.stamp = t2.stamp
        && Compilation_unit.equal t1.compilation_unit t2.compilation_unit

  let output chan t =
    output_string chan (Compilation_unit.string_for_printing t.compilation_unit);
    output_string chan "_";
    output_string chan (string_of_stamp t.stamp)

  let hash t =
    let stamp_hash =
      match t.stamp with
      | Stub -> 0
      | otherwise -> get_stamp_exn otherwise 
    in
    stamp_hash lxor (Compilation_unit.hash t.compilation_unit)

  let print ppf t =
    Format.fprintf ppf "Apply_id[%a/%s]"
      Compilation_unit.print t.compilation_unit
      (string_of_stamp t.stamp)
end)

let change_label t label =
  match t.stamp, label with
  | _, `Stub -> { t with stamp = Stub }
  | _, `Plain_apply -> { t with stamp = Plain_apply (get_stamp_exn t.stamp) }
  | _, `Over_application -> { t with stamp = Over_application (get_stamp_exn t.stamp) }

let previous_stamp = ref 0

let create ?current_compilation_unit ~parents label =
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let stamp = 
    match label with
    | `Stub -> Stub
    | `Plain_apply -> 
      incr previous_stamp;
      Plain_apply !previous_stamp
    | `Over_application -> 
      incr previous_stamp;
      Over_application !previous_stamp
  in
  { compilation_unit; stamp; parents; }
;;

let create_old ?current_compilation_unit label =
  create ?current_compilation_unit ~parents:None label
;;

let create ?current_compilation_unit label =
  create ?current_compilation_unit ~parents:(Some []) label
;;

let in_compilation_unit t c = Compilation_unit.equal c t.compilation_unit

let get_compilation_unit t = t.compilation_unit

let get_inlining_path t =
  let rec loop ~(acc : (Compilation_unit.t * stamp) list) (apply_id : t) =
    let parents =
      match apply_id.parents with
      | None -> Misc.fatal_error "This apply_id does not support parents"
      | Some x ->  x
    in
    List.fold_right (fun parent acc -> loop ~acc parent)
       parents ((apply_id.compilation_unit, apply_id.stamp) :: acc)
  in
  loop ~acc:[] t
;;

let inline ~caller ~inlined =
  match inlined.parents with
  | None -> inlined
  | Some parents_of_inlined ->
    let parents = Some (caller :: parents_of_inlined) in
    { inlined with parents }
;;
