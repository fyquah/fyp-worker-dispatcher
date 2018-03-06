[@@@ocaml.warning "+a-4-9-30-40-41-42-44"]

open Core

module Data_collector = Fyp_compiler_lib.Data_collector.V0
module Results = Protocol.Results

type exec_time =
  { exec_times : float Array.t;
    mean       : float;
    id         : Results.Work_unit_id.t;
  }

let selection all_results =
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
            let alpha = 0.4 in
            assert false
            (*
            let side = Owl.Stats.BothSide in
            let (h, _, _) =
              Owl.Stats.t_test_unpaired ~equal_var:false ~alpha ~side
                null.exec_times alternate.exec_times
            in
            not h || (null.mean < alternate.mean)
            *)
            ))
    in
    if List.length new_list = List.length exec_times
    then new_list
    else loop new_list
  in
  let exec_times =
    List.map all_results ~f:(fun (results : Results.t) ->
      let exec_times =
        Array.of_list_map results.benchmark.raw_execution_time
          ~f:Time.Span.to_sec
      in
      let id = results.id in
      let mean = Common.arithmetic_mean (Array.to_list exec_times) in
      { id; exec_times; mean; })
  in
  let filtered_exec_times = loop exec_times in
  let selected_ids =
    Results.Work_unit_id.Set.of_list (
      List.map filtered_exec_times ~f:(fun a -> a.id)
    )
  in
  List.filter all_results ~f:(fun (results : Results.t) ->
    Results.Work_unit_id.Set.mem selected_ids results.id)

let mutate (results : Results.t list) ~old_base =
  let new_stuff =
    (* quadrative time, but who cares? *)
    List.fold results ~init:[] ~f:(fun acc result ->
      List.fold ~init:acc result.overrides ~f:(fun acc needle ->
        let exists =
          List.exists acc ~f:(Data_collector.equal needle)
          || List.exists old_base ~f:(Data_collector.equal needle)
        in
        if exists
        then acc
        else needle :: acc))
  in
  match new_stuff with
  | [] -> None
  | otherwise -> Some (otherwise @ old_base)
