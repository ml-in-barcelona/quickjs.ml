(** TC39 Test262 Specification Tests for quickjs.ml

    This test suite is based on https://github.com/tc39/test262 Tests are
    manually translated from JavaScript to OCaml.

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
      (* Number tests *)
      ("Number.prototype.toString", Number.Prototype.To_string.tests);
      ("Number.prototype.toFixed", Number.Prototype.To_fixed.tests);
      ("Number.prototype.toPrecision", Number.Prototype.To_precision.tests);
      ("Number.prototype.toExponential", Number.Prototype.To_exponential.tests);
      (* parseFloat tests *)
      ("parseFloat", Parse_float.Parse.tests);
      (* String tests *)
      ("String.prototype.toLowerCase", String.Prototype.To_lower_case.tests);
      ("String.prototype.toUpperCase", String.Prototype.To_upper_case.tests);
      ("String.prototype.normalize", String.Prototype.Normalize.tests);
      ("Unicode.properties", String.Unicode_properties.tests);
    ]
