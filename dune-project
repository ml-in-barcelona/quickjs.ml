(lang dune 3.8)
(using directory-targets 0.1)
(using ctypes 0.3)

(cram enable)

(name quickjs)

(license MIT)

(maintainers "David Sancho <dsnxmoreno@gmail.com>")

(authors "David Sancho <dsnxmoreno@gmail.com>")

(source
 (github ml-in-barcelona/quickjs.ml))

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name quickjs)
 (synopsis "Bindings for QuickJS (a Javascript Engine to be embedded https://bellard.org/quickjs)")
 (depends
  ;; General system dependencies
  (ocaml (>= 4.14.0))
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
