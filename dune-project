(lang dune 3.0)
(using menhir 2.1)

(name camellia)

(package
 (name camellia)
 (synopsis "Zero-Knowledge Circuit DSL")
 (description "A domain-specific language for defining and compiling zero-knowledge circuits")
 (depends
  ocaml
  dune
  menhir
  (alcotest :with-test)
  fmt
  yojson))