(lang dune 3.10)

(name OCamlLox)

(generate_opam_files true)

(source
 (github InfiniteRain/OCamlLox))

(authors "David Lysenko")

(maintainers "David Lysenko")

(license MIT)

(package
 (name OCamlLox)
 (synopsis "Lox implementation in OCaml")
 (description "Lox implementation in OCaml")
 (depends ocaml dune (alcotest :with-test))
 (tags
  (ocaml lox interpreter)))

