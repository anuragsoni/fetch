# Fetch

To pin the development version: `opam pin add fetch https://github.com/anuragsoni/fetch.git`

## Example
```ocaml
let uri = Uri.of_string "https://httpbin.org/get" in
match%lwt (get uri |> run) with
| Ok r -> (* do something with response *)
| Error e -> (* do something with error *)
```

# Local Development

## Using [esy](https://esy.sh/)

* Install [esy](https://esy.sh/docs/en/getting-started.html)
* Clone the repository
* esy install
* esy build

## Using [opam](https://github.com/ocaml/opam)

* Clone the repository
* Create local switch `opam switch create ./`
* `dune build`
