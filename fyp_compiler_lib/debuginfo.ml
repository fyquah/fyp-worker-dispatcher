(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing

type item = {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
}

type t = item list

let none = []

let is_none = function
  | [] -> true
  | _ :: _ -> false

let to_string dbg =
  match dbg with
  | [] -> ""
  | ds ->
    let items =
      List.map
        (fun d ->
           Printf.sprintf "%s:%d,%d-%d"
             d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end)
        ds
    in
    "{" ^ String.concat ";" items ^ "}"
