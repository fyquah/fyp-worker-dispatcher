open Core
open Async

let run ?(name="Anon") lock utilise_resource =
  Deferred.repeat_until_finished () (fun () ->
      match Nano_mutex.lock lock with
      | Ok () ->
        Log.Global.info "%s succeeded to acquire lock" name;
        Deferred.return (`Finished ())
      | Error _ ->
        Log.Global.info "%s xailed to acquire lock" name;
        Clock.after (Time.Span.of_sec (Random.float 2.0))
        >>| fun () -> (`Repeat ()))
  >>= fun () ->
  Monitor.protect utilise_resource ~finally:(fun () -> 
    Deferred.repeat_until_finished () (fun () ->
      match Nano_mutex.unlock lock with
      | Ok () ->
        Log.Global.info "%s released lock" name;
        Deferred.return (`Finished ())
      | Error _ ->
        Log.Global.info "%s failed to release lock" name;
        Clock.after (Time.Span.of_sec (Random.float 2.0))
        >>| fun () -> `Repeat ()))
;;
