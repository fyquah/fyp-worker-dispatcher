module Results = Protocol.Results

val selection : Results.t list -> Results.t list

(* Really, this is just a fancy name for "SMASH EVERYTHING TOGETHER!!!!" *)
val mutate : Results.t list -> Fyp_compiler_lib.Data_collector.t list
