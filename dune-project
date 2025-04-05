(lang dune 3.16)

(name weaken)

(generate_opam_files true)

(source
 (github benmandrew/prop-goal-weakening))

(authors "Ben M. Andrew")

(maintainers "Ben M. Andrew")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name weaken)
 (synopsis "Propositional goal weakening")
 (description "Propositional goal weakening")
 (depends
  ocaml
  dune
  (why3 (>= 1.8.0))
  alt-ergo))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
