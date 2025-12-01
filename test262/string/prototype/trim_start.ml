(* TC39 Test262: String.prototype.trimStart tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/trimStart

   ECMA-262 Section: String.prototype.trimStart()

   Tests for String.Prototype.trim_start *)

module String = Quickjs.String

(* ===================================================================
   Basic trimStart functionality
   =================================================================== *)

let basic_trim_start () =
  assert_string (String.Prototype.trim_start "  hello  ") "hello  ";
  assert_string (String.Prototype.trim_start "hello") "hello";
  assert_string (String.Prototype.trim_start "   ") ""

let various_whitespace () =
  assert_string (String.Prototype.trim_start "  hello") "hello";
  assert_string (String.Prototype.trim_start "\thello") "hello";
  assert_string (String.Prototype.trim_start "\nhello") "hello";
  assert_string (String.Prototype.trim_start " \t\n hello") "hello"

let preserves_trailing () =
  assert_string (String.Prototype.trim_start "  hello  ") "hello  ";
  assert_string (String.Prototype.trim_start "hello  ") "hello  "

let empty_string () =
  assert_string (String.Prototype.trim_start "") ""

let no_leading_whitespace () =
  assert_string (String.Prototype.trim_start "hello  ") "hello  "

let unicode_whitespace () =
  assert_string (String.Prototype.trim_start "\u{00A0}hello") "hello";
  assert_string (String.Prototype.trim_start "\u{2003}hello") "hello"

let unicode_content () =
  assert_string (String.Prototype.trim_start "  日本語") "日本語";
  assert_string (String.Prototype.trim_start "  😀") "😀"

let tests =
  [
    test "trimStart: basic" basic_trim_start;
    test "trimStart: various whitespace" various_whitespace;
    test "trimStart: preserves trailing" preserves_trailing;
    test "trimStart: empty string" empty_string;
    test "trimStart: no leading whitespace" no_leading_whitespace;
    test "trimStart: Unicode whitespace" unicode_whitespace;
    test "trimStart: Unicode content" unicode_content;
  ]

