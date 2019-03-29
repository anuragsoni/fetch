open Httpaf

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

val get : ?headers:Headers.t -> Uri.t -> t

val post : ?headers:Headers.t -> ?body:string -> Uri.t -> t

val put : ?headers:Headers.t -> ?body:string -> Uri.t -> t

val run : t -> (response, error) Lwt_result.t
