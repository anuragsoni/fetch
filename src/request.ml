open Core
open Async

type t =
  { m : Httpaf.Method.t
  ; uri : Uri.t
  ; headers : Httpaf.Headers.t }

let uri t = t.uri

let m t = t.m

let headers t = t.headers

let get_port u = match Uri.port u with Some p -> p | None -> 80

let as_string b = b >>| fun buf -> Bytes.to_string (Buffer.to_bytes buf)

let create m uri =
  { m
  ; uri
  ; headers = Httpaf.Headers.of_list [("host", Uri.host_with_default uri)] }


let add_header ~key ~value t =
  {t with headers = Httpaf.Headers.add t.headers key value}


let response_handler finished response response_body =
  match response with
  | {Httpaf.Response.status = `OK; _} ->
      let b = Buffer.create 1028 in
      let rec on_read bs ~off ~len =
        let b' = Bytes.create len in
        Bigstringaf.blit_to_bytes bs ~src_off:off b' ~dst_off:0 ~len ;
        Buffer.add_bytes b b' ;
        Httpaf.Body.schedule_read response_body ~on_read ~on_eof
      and on_eof () = Ivar.fill finished b in
      Httpaf.Body.schedule_read response_body ~on_read ~on_eof
  | resp ->
      failwith
        (String.concat
           [ "Todo: do something better here. "
           ; Httpaf.Status.to_string resp.status
           ; ". "
           ; resp.reason ])


let run req =
  let {uri; headers; m} = req in
  let host = Uri.host_with_default uri in
  let port = get_port uri in
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port {host; port} in
  let finished = Ivar.create () in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
  let request_body =
    Httpaf_async.Client.request
      ~error_handler:(fun e ->
        match e with
        | `Exn e ->
            raise e
        | `Invalid_response_body_length _ ->
            failwith "invalid response length"
        | `Malformed_response r ->
            failwith r )
      ~response_handler:(response_handler finished)
      socket
      (Httpaf.Request.create ~headers m (Uri.path_and_query uri))
  in
  Httpaf.Body.close_writer request_body ;
  Ivar.read finished
