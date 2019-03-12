module Request : sig
  type t

  val uri : t -> Uri.t

  val m : t -> Httpaf.Method.t

  val headers : t -> Httpaf.Headers.t

  val get_port : Uri.t -> int

  val as_string : Buffer.t Lwt.t -> string Lwt.t

  val create : Httpaf.Method.t -> Uri.t -> t

  val add_header : key:string -> value:string -> t -> t

  val add_body : string option -> t -> t

  val run : t -> Buffer.t Lwt.t
end
