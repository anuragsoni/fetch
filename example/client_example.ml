open Core
open Async
open Fetch

let main port m host () =
  let m' = Httpaf.Method.of_string m in
  let u = Uri.with_port (Uri.of_string host) (Some port) in
  Request.create m' u
  |> Request.run
  |> Request.as_string
  >>| fun resp -> print_endline resp


let () =
  Command.async_spec
    ~summary:"Start a hello world Async client"
    Command.Spec.(
      empty
      +> flag "-p" (optional_with_default 80 int) ~doc:"int destination port"
      +> flag "-m" (required string) ~doc:"string HTTP method"
      +> flag "-h" (required string) ~doc:"string destination host")
    main
  |> Command.run
