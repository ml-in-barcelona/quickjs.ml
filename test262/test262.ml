(** TC39 Test262 Specification Tests for quickjs.ml

    This test suite is based on https://github.com/tc39/test262
    Tests are manually translated from JavaScript to OCaml.

    Structure mirrors tc39/test262/test/built-ins/ *)

let () =
  Alcotest.run "test262"
    [
      (* RegExp tests *)
      ("RegExp.compile", Regexp.Compile.tests);
      ("RegExp.prototype.test", Regexp.Test.tests);
      ("RegExp.prototype.exec", Regexp.Exec.tests);
      ("RegExp.flags", Regexp.Flags.tests);
      ("RegExp.source", Regexp.Source.tests);
    ]
