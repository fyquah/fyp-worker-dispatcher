open Core

type params =
  { config_filename   : string;
    controller_rundir : string;
    exp_dir           : string;
    bin_name          : string;
    bin_args          : string;
    module_paths      : string list;
  }

let params =
  let open Command.Let_syntax in
  [%map_open
     let config_filename =
       flag "-config" (required file) ~doc:"PATH to config file"
     and controller_rundir =
       flag "-rundir" (required string) ~doc:"PATH rundir"
     and exp_dir =
       flag "-exp-dir" (required string) ~doc:"PATH experiment directory"
     and bin_name =
       flag "-bin-name" (required string)
         ~doc:"STRING binary name (without the .ml extension!)"
     and bin_args =
       flag "-args" (required string) ~doc:"STRING arguments"
     and debug = flag "-debug" no_arg ~doc:"debug"
     and module_paths =
       flag "-module-paths" (required string) ~doc:"STRING comma separated list of modules"
     in
     if Filename.check_suffix bin_name ".ml" then begin
       failwith "Binary name should not contain .ml suffix!"
     end;
     if debug then begin
       Common.set_shell_defaults ~echo:true ~verbose:true ()
     end;
     Random.init (
        (Time.to_span_since_epoch (Time.now ()))
        |> Time.Span.to_sec
        |> Float.to_int
     );
     Experiment_utils.Dump_utils.set_controller_rundir controller_rundir;
     let module_paths = String.split ~on:',' module_paths in
     { config_filename; controller_rundir; exp_dir; bin_name; bin_args;
       module_paths; }]
