(lang dune 3.7)

(generate_opam_files true)

(name ostap)

(license LGPL-2.1-or-later)

; (maintainer "")

(package
 (name ostap)
 (version 0.6)
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
  GT
  re
  (ocamlfind :build)
  (camlp5
   (>= 8))))
