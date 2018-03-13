(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Identifiable.S

val create : Closure_id.t -> t

val unknown : t

val get_name : t -> string
val get_compilation_unit : t -> Compilation_unit.t
val rename : (Closure_id.t -> Closure_id.t) -> t -> t

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t
