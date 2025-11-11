(lang dune 3.20)

(name lab1)

(generate_opam_files true)

(package
 (name lab1)
 (synopsis "Lab 1: Project Euler problems")
 (description "Solutions to Project Euler problems 12 and 18")
 (maintainers "vladimir melnikov")
 (authors "vladimir melnikov")
 (license "apache-2.0")
 (homepage "https://github.com/Vovanm88/fp-25")
 (bug_reports "https://github.com/Vovanm88/fp-25/issues")
 (depends
  ocaml
  (ocamlformat :dev)))
