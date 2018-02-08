open Core
open Async

let run ?(name="Anon") lock utilise_resource =
  Deferred.repeat_until_finished () (fun () ->
      match Nano_mutex.lock lock with
      | Ok () ->
        Log.Global.info "%s succeeded to acquire lock" name;
        Deferred.return (`Finished ())
      | Error _ ->
        Log.Global.debug "%s xailed to acquire lock" name;
        Clock.after (Time.Span.of_sec 1.0) >>| fun () -> (`Repeat ()))
  >>= fun () -> utilise_resource ()
  >>= fun ret ->
  Deferred.repeat_until_finished () (fun () ->
    match Nano_mutex.unlock lock with
    | Ok () ->
      Log.Global.info "%s released lock" name;
      Deferred.return (`Finished ())
    | Error _ ->
      Log.Global.debug "%s failed to release lock" name;
      Clock.after (Time.Span.of_sec 1.0) >>| fun () -> `Repeat ())
  >>| fun () -> ret
