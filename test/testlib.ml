open Core
open Async


let all_tests = ref []

let repo_root = "../../.."

let register (module M: Test_intf.S) =
  all_tests := (module M : Test_intf.S) :: !all_tests
;;

let run () =
  let success = ref [] in
  let failed = ref [] in
  Deferred.List.iter !all_tests ~how:`Sequential
    ~f:(fun (module M : Test_intf.S) ->
      printf "===================================\n";
      printf "===> Executing test %s\n" M.name;
      printf "===================================\n";
      Monitor.try_with M.run
      >>| function
      | Error exn -> 
        printf "=======> %s failed <=========\n" M.name;
        printf "%s\n" (Exn.to_string exn);
        failed := M.name :: !failed
      | Ok () -> 
        printf "=======> %s passed <=========\n" M.name;
        success := M.name :: !success)
  >>| fun () ->
  printf "Succeeded tests:\n";
  List.iter !success ~f:(fun t -> printf " - %s\n" t);
  printf "Failed tests:\n";
  List.iter !failed ~f:(fun t -> printf " - %s\n" t);
  printf "---------------------------\n"
;;
