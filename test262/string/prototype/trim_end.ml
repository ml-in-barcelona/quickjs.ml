(* TC39 Test262: String.prototype.trimEnd tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/trimEnd

   ECMA-262 Section: String.prototype.trimEnd()

   Tests for String.Prototype.trim_end *)

module String = Quickjs.String

(* ===================================================================
   Basic trimEnd functionality
   =================================================================== *)

let basic_trim_end () =
  assert_string (String.Prototype.trim_end "  hello  ") "  hello";
  assert_string (String.Prototype.trim_end "hello") "hello";
  assert_string (String.Prototype.trim_end "   ") ""

let various_whitespace () =
  assert_string (String.Prototype.trim_end "hello  ") "hello";
  assert_string (String.Prototype.trim_end "hello\t") "hello";
  assert_string (String.Prototype.trim_end "hello\n") "hello";
  assert_string (String.Prototype.trim_end "hello \t\n ") "hello"

let preserves_leading () =
  assert_string (String.Prototype.trim_end "  hello  ") "  hello";
  assert_string (String.Prototype.trim_end "  hello") "  hello"

let empty_string () =
  assert_string (String.Prototype.trim_end "") ""

let no_trailing_whitespace () =
  assert_string (String.Prototype.trim_end "  hello") "  hello"

let unicode_whitespace () =
  assert_string (String.Prototype.trim_end "hello\u{00A0}") "hello";
  assert_string (String.Prototype.trim_end "hello\u{2003}") "hello"

let unicode_content () =
  assert_string (String.Prototype.trim_end "日本語  ") "日本語";
  assert_string (String.Prototype.trim_end "😀  ") "😀"

let tests =
  [
    test "trimEnd: basic" basic_trim_end;
    test "trimEnd: various whitespace" various_whitespace;
    test "trimEnd: preserves leading" preserves_leading;
    test "trimEnd: empty string" empty_string;
    test "trimEnd: no trailing whitespace" no_trailing_whitespace;
    test "trimEnd: Unicode whitespace" unicode_whitespace;
    test "trimEnd: Unicode content" unicode_content;
  ]

