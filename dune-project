(lang dune 3.20)

(name lab3)

(generate_opam_files true)

(package
 (name lab3)
 (synopsis "Lab 3: Streaming interpolation")
 (description "Streaming linear interpolation with command-line interface")
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
