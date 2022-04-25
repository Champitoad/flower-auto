(lang dune 3.0)
(using menhir 2.1)
(name prover)

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
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
