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
  (integers (>= 0.7.0))
  ;; 0.24.0 is the first release whose interface uses Bigarray directly
  ;; instead of Bigarray_compat, which we rely on in RegExp.ml. Older
  ;; releases fail to typecheck without a bigarray-compat dependency.
  (ctypes (>= 0.24.0))

  ;; Test dependencies
  (alcotest :with-test)

  ;; Documentation
  (odoc :with-doc)

  ;; Dev dependencies
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    (= 0.28.1)
    :with-dev-setup))
))
