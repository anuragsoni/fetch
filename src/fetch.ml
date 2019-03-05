open Core
open Async
open Httpaf
open Httpaf_async

let response_handler finished response response_body =
  match response with
  | {Response.status= `OK; _} ->
      let b = Buffer.create 1028 in
      let rec on_read bs ~off ~len =
        let b' = Bytes.create len in
        Bigstringaf.blit_to_bytes bs ~src_off:off b' ~dst_off:0 ~len ;
        Buffer.add_bytes b b' ;
        Body.schedule_read response_body ~on_read ~on_eof
      and on_eof () =
        Ivar.fill finished (Bytes.to_string (Buffer.to_bytes b))
      in
      Body.schedule_read response_body ~on_read ~on_eof
  | _response -> failwith ""

module Request = struct
  let get_port u = match Uri.port u with Some p -> p | None -> 80

  let create (m : Method.t) u =
    let host = Uri.host_with_default u in
    let port = get_port u in
    let where_to_connect =
      Tcp.Where_to_connect.of_host_and_port {host; port}
    in
    let finished = Ivar.create () in
    Tcp.connect_sock where_to_connect
    >>= fun socket ->
    let headers = Headers.of_list [("host", host)] in
    let request_body =
      Client.request
        ~error_handler:(fun _ -> assert false)
        ~response_handler:(response_handler finished)
        socket
        (Request.create ~headers m (Uri.path_and_query u))
    in
    Body.close_writer request_body ;
    Ivar.read finished
end
