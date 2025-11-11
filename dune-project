(lang dune 3.20)

(name lab1)

(generate_opam_files true)

(package
 (name lab1)
 (synopsis "Lab 1: Project Euler problems")
 (description "Solutions to Project Euler problems 12 and 18")
 (depends
  ocaml
  (ocamlformat :dev)))
