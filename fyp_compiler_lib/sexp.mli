type t =
  | Atom of string
  | List of t list

exception Parse_error of string

val print_mach : Format.formatter -> t -> unit

val t_of_list : ('a -> t) -> 'a list -> t
val list_of_t : (t -> 'a) -> t -> 'a list

val sexp_of_list : ('a -> t) -> 'a list -> t
val list_of_sexp : (t -> 'a) -> t -> 'a list

val t_of_bool : bool -> t
val bool_of_t : t -> bool

val string_of_t : t -> string
val t_of_string : string -> t

val int_of_t : t -> int
val t_of_int : int -> t
