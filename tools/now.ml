open Core

let () = printf "%s" (Time.to_string_iso8601_basic ~zone:Time.Zone.utc (Time.now ()))
