open Core
open Async


let all_tests = ref []

let repo_root = "../../.."

let register (module M: Test_intf.S) =
  all_tests := (module M : Test_intf.S) :: !all_tests
;;

let run () =
  Deferred.List.iter !all_tests ~how:`Sequential
    ~f:(fun (module M : Test_intf.S) ->
      printf "==> Executing test %s\n" M.name;
      M.run ())
;;
