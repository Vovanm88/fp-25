(lang dune 3.20)

(name lab2)

(generate_opam_files true)

(package
 (name lab2)
 (synopsis "Lab 2: Red-Black Tree Set implementation")
 (description "Implementation of red-black tree set data structure")
 (maintainers "vladimir melnikov")
 (authors "vladimir melnikov")
 (license "apache-2.0")
 (homepage "https://github.com/Vovanm88/fp-25")
 (bug_reports "https://github.com/Vovanm88/fp-25/issues")
 (depends
  ocaml
  (ocamlformat :dev)
  (qcheck :with-test)
  (alcotest :with-test)))
