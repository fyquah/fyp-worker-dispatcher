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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Closure_id

let unknown =
  let current_compilation_unit =
    let ident = Ident.create_persistent "unknown" in
    let linkage_name = Linkage_name.create "unknown" in
    Compilation_unit.create ident linkage_name
  in
  wrap (Variable.create ~current_compilation_unit "unknown")
;;

let create t = t
let rename f t = f t

let sexp_of_t t = Closure_id.sexp_of_t t
let t_of_sexp s = Closure_id.t_of_sexp s
