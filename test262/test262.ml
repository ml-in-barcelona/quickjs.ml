(** TC39 Test262 Specification Tests for quickjs.ml

    This test suite is based on https://github.com/tc39/test262
    Tests are manually translated from JavaScript to OCaml.

    Structure mirrors tc39/test262/test/built-ins/ *)

let () =
  Alcotest.run "test262"
    [
      (* Number tests *)
      ("Number.parseInt", Number.Parse_int.tests);
      ("Number.parseFloat", Number.Parse_float.tests);
      ("Number.isNaN", Number.Is_nan.tests);
      ("Number.isFinite", Number.Is_finite.tests);
      ("Number.isInteger", Number.Is_integer.tests);
      (* RegExp tests *)
      ("RegExp.compile", Regexp.Compile.tests);
      ("RegExp.prototype.test", Regexp.Test.tests);
      ("RegExp.prototype.exec", Regexp.Exec.tests);
      ("RegExp.flags", Regexp.Flags.tests);
      ("RegExp.source", Regexp.Source.tests);
    ]
