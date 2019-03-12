open Lwt.Infix

type t =
  { m : Httpaf.Method.t
  ; uri : Uri.t
  ; headers : Httpaf.Headers.t
  ; body : string option }

let uri t = t.uri

let m t = t.m

let headers t = t.headers

let get_port u = match Uri.port u with Some p -> p | None -> 80

let respond_as_string b =
  Lwt.map (fun buf -> Bytes.to_string (Buffer.to_bytes buf)) b


let create m uri =
  { m
  ; uri
  ; headers = Httpaf.Headers.of_list [("host", Uri.host_with_default uri)]
  ; body = None }


let add_header ~key ~value t =
  {t with headers = Httpaf.Headers.add t.headers key value}


let add_body body t = {t with body}

let response_handler finished response response_body =
  match response with
  | {Httpaf.Response.status = `OK; _} ->
      let b = Buffer.create 1028 in
      let rec on_read bs ~off ~len =
        let b' = Bytes.create len in
        Bigstringaf.blit_to_bytes bs ~src_off:off b' ~dst_off:0 ~len ;
        Buffer.add_bytes b b' ;
        Httpaf.Body.schedule_read response_body ~on_read ~on_eof
      and on_eof () = Lwt.wakeup finished b in
      Httpaf.Body.schedule_read response_body ~on_read ~on_eof
  | resp ->
      failwith
        (String.concat
           ""
           [ "Todo: do something better here. "
           ; Httpaf.Status.to_string resp.status
           ; ". "
           ; resp.reason ])


let run req =
  let {uri; headers; m; _} = req in
  let body = match req.body with Some body -> body | None -> "" in
  let headers =
    Httpaf.Headers.add_unless_exists
      headers
      "Content-Length"
      (string_of_int (String.length body))
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
      ~error_handler:(fun e ->
        match e with
        | `Exn e ->
            raise e
        | `Invalid_response_body_length _ ->
            failwith "invalid response length"
        | `Malformed_response r ->
            failwith r )
      ~response_handler:(response_handler notify_response_received)
      socket
      (Httpaf.Request.create ~headers m (Uri.path_and_query uri))
  in
  Httpaf.Body.write_string request_body body ;
  Httpaf.Body.close_writer request_body ;
  response_received
