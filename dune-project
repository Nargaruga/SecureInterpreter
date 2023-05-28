(lang dune 3.7)

(name secure_interpreter)

(generate_opam_files true)

(authors "Leonardo Da Pozzo")

(using menhir 2.1)

(package
 (name secure_interpreter)
 (synopsis "LBTS Assignment 1")
 (description
  "Interpreter for a functional programming language equipped with primitives for isolating sensitive data from untrusted code.")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
