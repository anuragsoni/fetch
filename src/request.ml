open Lwt.Infix

let src = Logs.Src.create "fetch"

type t =
  { m : Httpaf.Method.t
  ; uri : Uri.t
  ; req : Httpaf.Request.t
  ; body : string option }

type error = Http of Httpaf.Client_connection.error

let uri t = t.uri

let m t = t.m

let get_port u = match Uri.port u with Some p -> p | None -> 80

let create m uri headers body =
  let req = Httpaf.Request.create ~headers m (Uri.path_and_query uri) in
  {m; uri; req; body}


let get ?headers:(h = Httpaf.Headers.empty) uri = create `GET uri h None

let post ?headers:(h = Httpaf.Headers.empty) ?body uri =
  create `POST uri h body


let put ?headers:(h = Httpaf.Headers.empty) ?body uri = create `PUT uri h body

let add_body body t = {t with body}

let response_handler finished response response_body =
  Logs.debug ~src (fun m -> m "%a" Httpaf.Response.pp_hum response) ;
  match response with
  | {Httpaf.Response.status = `OK; _} ->
      let b = Buffer.create 1028 in
      let rec on_read bs ~off ~len =
        let b' = Bytes.create len in
        Bigstringaf.blit_to_bytes bs ~src_off:off b' ~dst_off:0 ~len ;
        Buffer.add_bytes b b' ;
        Httpaf.Body.schedule_read response_body ~on_read ~on_eof
      and on_eof () = Lwt.wakeup_later finished (Ok (Buffer.contents b)) in
      Httpaf.Body.schedule_read response_body ~on_read ~on_eof
  | resp ->
      Logs.err ~src (fun m ->
          m
            "TODO: do something better here. Status code %s. Reason: %s"
            (Httpaf.Status.to_string resp.status)
            resp.reason ) ;
      Lwt.wakeup_later finished (Error "TODO: Do something better")


let run t =
  let {uri; req; _} = t in
  let body = match t.body with Some body -> body | None -> "" in
  let headers =
    Httpaf.Headers.add_list
      req.headers
      [ ("User-Agent", "fetch")
      ; ("Host", Uri.host_with_default uri)
      ; ("Content-Length", string_of_int (String.length body)) ]
  in
  let host = Uri.host_with_default uri in
  let port = get_port uri in
  Lwt_unix.getaddrinfo host (string_of_int port) [Unix.(AI_FAMILY PF_INET)]
  >>= fun addresses ->
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr
  >>= fun () ->
  let response_received, notify_response_received = Lwt.wait () in
  let request_body =
    Httpaf_lwt.Client.request
      ~error_handler:(fun _e ->
        Lwt.wakeup_later
          notify_response_received
          (Error "TODO: do something better here") )
      ~response_handler:(response_handler notify_response_received)
      socket
      {t.req with headers}
  in
  Httpaf.Body.write_string request_body body ;
  Httpaf.Body.close_writer request_body ;
  response_received >>= Lwt_result.lift
