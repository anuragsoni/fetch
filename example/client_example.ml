open Fetch

let main () =
  let log_response resp =
    match resp with
    | Ok r ->
        Logs_lwt.info (fun m -> m "%s\n" r)
    | Error e ->
        Logs_lwt.err (fun m -> m "%s\n" e)
  in
  let get_url = Uri.of_string "http://httpbin.org/get" in
  let post_url = Uri.of_string "http://httpbin.org/post" in
  let%lwt get_response = Request.get get_url |> Request.run
  and post_response = Request.post ~body:"Foobar" post_url |> Request.run in
  let%lwt () = log_response get_response
  and () = log_response post_response in
  Lwt.return ()


let () =
  Logs.set_level (Some Logs.Info) ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Fmt_tty.setup_std_outputs () ;
  Lwt_main.run
    (Lwt.bind (main ()) (fun () ->
         Logs_lwt.info (fun m -> m "Finished all HTTP calls.") ))
