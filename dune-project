(lang dune 3.0)
(using menhir 2.1)
(name prover)
(version df7b390-dirty)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Pablo Donato")

(maintainers "Pablo Donato")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name prover)
 (synopsis "A simple prover for the flower calculus")
 (description "A longer description")
 (depends ocaml dune lwt menhirLib batteries tyxml ocamlgraph tyxml-ppx lwt_ppx)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
