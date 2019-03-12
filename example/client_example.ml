open Fetch

let ( >>| ) = Lwt.map

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


let main port m host () =
  let m' = Httpaf.Method.of_string m in
  let u = Uri.with_port (Uri.of_string host) (Some port) in
  Request.create m' u |> Request.run |> Request.as_string


let () =
  Logs.set_level (Some Logs.Info) ;
  Logs.set_reporter (lwt_reporter ()) ;
  let host = ref None in
  let port = ref 80 in
  Arg.parse
    [("-p", Set_int port, " Port number (80 by default)")]
    (fun host_argument -> host := Some host_argument)
    "get_example.exe [-p N] HOST" ;
  let host =
    match !host with
    | None ->
        failwith "No hostname provided"
    | Some host ->
        host
  in
  Lwt_main.run
    (Lwt.bind (main !port "GET" host ()) (fun response ->
         Logs_lwt.info (fun m -> m "Responded with %s" response) ))
