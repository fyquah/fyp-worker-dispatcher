open Async
open Core
open Protocol.Shadow_fyp_compiler_lib

val load_queries
   : ?allow_repeat:unit
  -> filelist: string list
  -> Inlining_query.query Pipe.Reader.t
