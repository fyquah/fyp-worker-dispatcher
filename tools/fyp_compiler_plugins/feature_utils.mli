open Import

module Feature_list : sig
  type 'a t

  val empty : 'a t

  val concat : 'a t -> 'a t -> 'a t

  val data : 'a t -> 'a list

  val of_list : (string * 'a) list -> 'a t

  val to_list : 'a t -> (string * 'a) list
end

type normaliser =
  { mean : float Feature_list.t;
    std  : float Feature_list.t;
  }

module Features : sig
  type 'a t =
    { int_features     : int   Feature_list.t;
      numeric_features : float Feature_list.t;
      bool_features    : bool  Feature_list.t;
      metadata         : string list;
    }

  val concat : 'a t -> 'a t -> 'a t

  val (@) : 'a t -> 'a t -> 'a t

  val names : 'a t -> string list

  val find_exn
    : 'a t -> string -> [ `Int of int | `Numeric of float | `Bool of bool ]

  val find_numeric_exn : 'a t -> string -> float

  val verify_fields : 'a t list -> bool

  val to_array : 'a t -> float array

  val create_normaliser : 'a t list -> normaliser

  val create_normaliser_function
     : normaliser -> ([ `Staged of ([ `raw ] t -> [ `normalised ] t) ])
end

val query_trace : Inlining_query.query -> Feature_extractor.trace_item list
