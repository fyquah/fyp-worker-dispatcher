open Core
open Async

module Test = struct
  let name = "regex-test"

  let run () =
    let regex = Re2.Regex.create_exn "^(\\d+\\.\\d+)\\s*user" in
    let haystack =
      "1.84user 0.00system 0:01.84elapsed 100%CPU (0avgtext+0avgdata 18832maxresident)"
    in
    let m = Re2.Regex.first_match_exn regex haystack in
    printf "%s" (Re2.Regex.Match.get_exn ~sub:(`Index 0) m);
    Deferred.unit
end

let () =
  Testlib.register (module Test)
