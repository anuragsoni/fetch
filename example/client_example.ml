let main () =
  let log_response resp =
    match resp with
    | Ok {Fetch.body;_} ->
        Logs_lwt.info (fun m -> m "%s\n" body)
    | Error _ ->
        Logs_lwt.err (fun m -> m "Error\n")
  in
  let get_url = Uri.of_string "http://httpbin.org/get" in
  let post_url = Uri.of_string "http://httpbin.org/post" in
  let%lwt get_response = Fetch.get get_url |> Fetch.run
  and post_response = Fetch.post ~body:"Foobar" post_url |> Fetch.run in
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
