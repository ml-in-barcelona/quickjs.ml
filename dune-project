(lang dune 3.8)
(using directory-targets 0.1)
(using ctypes 0.3)
(using mdx 0.4)

(cram enable)

(name regexp)

(license MIT)

(maintainers "David Sancho <dsnxmoreno@gmail.com>")

(authors "David Sancho <dsnxmoreno@gmail.com>")

(source
 (github ml-in-barcelona/regexp))

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name regexp)
 (synopsis "RegExp library for ReasonML and OCaml, binds to the native JavaScript RegExp from C")
 (depends
  ;; General system dependencies
  (ocaml (>= 5.0.0))
  (reason (>= 3.10.0))
  integers
  ctypes

  ;; Test dependencies
  (alcotest :with-test)
  (fmt :with-test)

  ;; Documentation
  (odoc :with-doc)

  ;; Dev dependencies, using with-test so that consumers don't install them (until package is released in opam)
  (ocaml-lsp-server :with-test)
  (ocamlformat
   (and
    (= 0.26.1)
    :with-test))
))
