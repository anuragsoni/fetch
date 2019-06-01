open Lwt.Infix
open Httpaf

let src = Logs.Src.create "fetch"

type response_body = string

type t =
  { m : Method.t
  ; uri : Uri.t
  ; req : Request.t
  ; body : string option
  }

type response =
  { res : Response.t
  ; body : response_body
  }

type error = Http of Client_connection.error

let get_port u =
  match Uri.port u with
  | Some p -> p
  | None -> 80
;;

let create m uri headers body =
  let req = Request.create ~headers m (Uri.path_and_query uri) in
  { m; uri; req; body }
;;

let get ?headers:(h = Headers.empty) uri = create `GET uri h None
let post ?headers:(h = Headers.empty) ?body uri = create `POST uri h body
let put ?headers:(h = Headers.empty) ?body uri = create `PUT uri h body

let run t =
  let response_received, notify_response_received = Lwt.wait () in
  let error_received, notify_error = Lwt.wait () in
  let response_handler response response_body =
    Logs.debug ~src (fun m -> m "%a" Response.pp_hum response);
    let b = Buffer.create 1028 in
    let rec on_read bs ~off ~len =
      let b' = Bytes.create len in
      Bigstringaf.blit_to_bytes bs ~src_off:off b' ~dst_off:0 ~len;
      Buffer.add_bytes b b';
      Body.schedule_read response_body ~on_read ~on_eof
    and on_eof () =
      Lwt.wakeup_later
        notify_response_received
        (Ok { res = response; body = Buffer.contents b })
    in
    Body.schedule_read response_body ~on_read ~on_eof
  in
  let { uri; req; _ } = t in
  let body =
    match t.body with
    | Some body -> body
    | None -> ""
  in
  let headers =
    Headers.add_list
      req.headers
      [ "User-Agent", "fetch"
      ; "Host", Uri.host_with_default uri
      ; "Content-Length", string_of_int (String.length body)
      ]
  in
  let host = Uri.host_with_default uri in
  let port = get_port uri in
  Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  >>= fun addresses ->
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr
  >>= fun () ->
  let request_body =
    Httpaf_lwt_unix.Client.request
      ~error_handler:(fun e -> Lwt.wakeup_later notify_error (Error (Http e)))
      ~response_handler
      socket
      { t.req with headers }
  in
  Body.write_string request_body body;
  Body.close_writer request_body;
  Lwt.pick [ response_received; error_received ] >>= Lwt_result.lift
;;
