(lang dune 3.7)

(generate_opam_files true)

(using mdx 0.3)

(name ostap)

(source
 (github PLTools/ostap))

(license LGPL-2.1-or-later)

(maintainers "Kakadu@pm.me")

(package
 (name ostap)
 (version 0.6.1)
 (synopsis "Parser-combinator library")
 (description "")
 (authors
  "dboulytchev@gmail.com"
  "danila.borovkov1996@gmail.com"
  "Kakadu@pm.me")
 (maintainers "Kakadu@pm.me")
 (homepage https://github.com/PLTools/ostap)
 (bug_reports "https://github.com/PLTools/ostap/issues")
 (depends
  (ocaml
   (or
    (and
     (>= 4.14)
     (< 5))
    (>= 5.3)))
  (re
   (>= 1.10))
  (camlp5
   (>= 8))
  GT
  (ocamlfind :build)
  (mdx :with-test)
  (odig :with-doc)
  (odoc :with-doc)
  (pa_ppx :with-doc)
  ;
  ))
