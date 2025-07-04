(lang dune 3.0)

(name camellia)

(package
 (name camellia)
 (synopsis "Zero-Knowledge Circuit DSL")
 (description "A domain-specific language for defining and compiling zero-knowledge circuits")
 (depends
  ocaml
  dune
  (alcotest :with-test)
  fmt))