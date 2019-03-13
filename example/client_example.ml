open Fetch

let ( >>| ) = Lwt.map

let ( >>=? ) = Lwt_result.bind

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b
    , fun () ->
        let m = Buffer.contents b in
        Buffer.reset b ;
        m )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App ->
            Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ ->
            Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () =
        over () ;
        Lwt.return_unit
      in
      Lwt.finalize write unblock |> Lwt.ignore_result ;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  {Logs.report}


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
  Logs.set_reporter (lwt_reporter ()) ;
  Lwt_main.run
    (Lwt.bind (main ()) (fun () ->
         Logs_lwt.info (fun m -> m "Finished all HTTP calls.") ))
