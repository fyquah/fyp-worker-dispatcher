[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core
open Async

module Results = Protocol.Results

type exec_time =
  { exec_times : float Array.t;
    mean       : float;
    id         : Results.Work_unit_id.t;
  }

let () =
  let open Command.Let_syntax in
  Command.async' ~summary:"Selection and mutation"
    [%map_open
     let result_filenames = anon (sequence ("filenames" %: string)) in
     let open Deferred.Let_syntax in
     fun () ->
       (* The semantics of the loop is written somewhere in the log book.
        * Informally:
          * exec times in selected transformations are statistically similar.
          * there exists at least a selected transformation that discarded
            a rejected transformation, for all rejected transformations
        *)
       let rec loop exec_times =
         let new_list =
           List.filter exec_times ~f:(fun null ->
             List.for_all exec_times ~f:(fun alternate ->
               if Results.Work_unit_id.compare null.id alternate.id = 0
               then true
               else
                 (* By increasing [alpha], we decrease the resultant set
                  * of selected inlining decisions.
                  *
                  * This is why alpha is set to such a rediculously high
                  * level (the standard for such statistics is 0.05) is to
                  * reduce the number of selected gnomes. The higher alpha
                  * is, the more likely we classify to experiments to have
                  * originated from different samples.
                  *
                  * By using a high alpha, we will accept less samples in
                  * the new set.
                  *
                  * TODO: This distribution is not really a gaussian, but
                  * rather, a one-sided gaussian -- I suspect this is because
                  * of the kernel's behaviour with buffering etc.
                  *)
                 let alpha = 0.3 in
                 let side = Owl.Stats.BothSide in
                 let (h, _, _) =
                   Owl.Stats.t_test_unpaired ~equal_var:false ~alpha ~side
                     null.exec_times alternate.exec_times
                 in
                 not h || (null.mean > alternate.mean)))
         in
         if List.length new_list = List.length exec_times
         then new_list
         else loop new_list
       in
       let%map exec_times =
         Deferred.List.map result_filenames ~f:(fun filename ->
           let%map results =
             Reader.load_sexp_exn filename [%of_sexp: Results.t]
           in
           let exec_times =
             Array.of_list_map results.benchmark.raw_execution_time
               ~f:Time.Span.to_sec
           in
           let id = results.id in
           let mean = Owl.Stats.mean exec_times in
           { id; exec_times; mean; })
       in
       let filtered_exec_times = loop exec_times in
       List.map filtered_exec_times ~f:(fun a -> Results.Work_unit_id.to_string (a.id))
       |> String.concat ~sep:", "
       |> printf "chosen ids = %s\n"
    ]
  |> Command.run
