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
      (* parseInt tests *)
      ("parseInt", Parse_int.Parse.tests);
      (* String case tests *)
      ("String.prototype.toLowerCase", String.Prototype.To_lower_case.tests);
      ("String.prototype.toUpperCase", String.Prototype.To_upper_case.tests);
      ("String.prototype.normalize", String.Prototype.Normalize.tests);
      ("Unicode.properties", String.Unicode_properties.tests);
      (* String character access tests *)
      ("String.prototype.charAt", String.Prototype.Char_at.tests);
      ("String.prototype.charCodeAt", String.Prototype.Char_code_at.tests);
      ("String.prototype.codePointAt", String.Prototype.Code_point_at.tests);
      (* String substring tests *)
      ("String.prototype.slice", String.Prototype.Slice.tests);
      ("String.prototype.substring", String.Prototype.Substring.tests);
      ("String.prototype.substr", String.Prototype.Substr.tests);
      (* String search tests *)
      ("String.prototype.indexOf", String.Prototype.Index_of.tests);
      ("String.prototype.lastIndexOf", String.Prototype.Last_index_of.tests);
      ("String.prototype.includes", String.Prototype.Includes.tests);
      ("String.prototype.startsWith", String.Prototype.Starts_with.tests);
      ("String.prototype.endsWith", String.Prototype.Ends_with.tests);
      (* String transform tests *)
      ("String.prototype.trim", String.Prototype.Trim.tests);
      ("String.prototype.trimStart", String.Prototype.Trim_start.tests);
      ("String.prototype.trimEnd", String.Prototype.Trim_end.tests);
      ("String.prototype.padStart", String.Prototype.Pad_start.tests);
      ("String.prototype.padEnd", String.Prototype.Pad_end.tests);
      ("String.prototype.repeat", String.Prototype.Repeat.tests);
      (* String regex tests *)
      ("String.prototype.match", String.Prototype.Match_.tests);
      ("String.prototype.matchAll", String.Prototype.Match_all.tests);
      ("String.prototype.replace", String.Prototype.Replace.tests);
      ("String.prototype.replaceAll", String.Prototype.Replace_all.tests);
      ("String.prototype.split", String.Prototype.Split.tests);
      ("String.prototype.search", String.Prototype.Search.tests);
    ]
