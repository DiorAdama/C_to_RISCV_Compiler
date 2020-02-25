# Install with opam
1. Install opam (Ocaml PAckage Manager)
    - https://opam.ocaml.org/doc/Install.html
2. Get sufficient version of ocaml
    > opam switch create 4.08.0 (or higher)
3. Install packages with opam 
    > opam install package-name
    - stdlib-shims
    - ocamlbuild
    - ocamlfind
    - menhir
    - lwt
    - logs
    - batteries
    - yojson
    - websocket
    - websocket-lwt-unix
4. Build the project
    > make
