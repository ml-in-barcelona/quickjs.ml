(* TC39 Test262: String.prototype.trim tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/trim

   ECMA-262 Section: String.prototype.trim()

   Tests for String.Prototype.trim *)

module String = Quickjs.String

(* ===================================================================
   Basic trim functionality - S15.5.4.20
   =================================================================== *)

let basic_trim () =
  assert_string (String.Prototype.trim "  hello  ") "hello";
  assert_string (String.Prototype.trim "hello") "hello";
  assert_string (String.Prototype.trim "   ") ""

let leading_whitespace () =
  assert_string (String.Prototype.trim "  hello") "hello";
  assert_string (String.Prototype.trim "\thello") "hello";
  assert_string (String.Prototype.trim "\nhello") "hello"

let trailing_whitespace () =
  assert_string (String.Prototype.trim "hello  ") "hello";
  assert_string (String.Prototype.trim "hello\t") "hello";
  assert_string (String.Prototype.trim "hello\n") "hello"

let mixed_whitespace () =
  assert_string (String.Prototype.trim " \t\n hello \t\n ") "hello";
  assert_string
    (String.Prototype.trim "\r\n\t hello world \t\r\n")
    "hello world"

let empty_string () = assert_string (String.Prototype.trim "") ""
let no_whitespace () = assert_string (String.Prototype.trim "hello") "hello"

let unicode_whitespace () =
  (* Various Unicode whitespace characters *)
  (* \u00A0 = non-breaking space *)
  assert_string (String.Prototype.trim "\u{00A0}hello\u{00A0}") "hello";
  (* \u2003 = em space *)
  assert_string (String.Prototype.trim "\u{2003}hello\u{2003}") "hello";
  (* \uFEFF = BOM/ZWNBSP *)
  assert_string (String.Prototype.trim "\u{FEFF}hello\u{FEFF}") "hello"

let preserves_internal_whitespace () =
  assert_string (String.Prototype.trim "  hello world  ") "hello world";
  assert_string (String.Prototype.trim "  hello\tworld  ") "hello\tworld"

let unicode_content () =
  assert_string (String.Prototype.trim "  日本語  ") "日本語";
  assert_string (String.Prototype.trim "  😀  ") "😀"

let tests =
  [
    test "S15.5.4.20_A1: basic trim" basic_trim;
    test "S15.5.4.20_A2: leading whitespace" leading_whitespace;
    test "S15.5.4.20_A3: trailing whitespace" trailing_whitespace;
    test "S15.5.4.20_A4: mixed whitespace" mixed_whitespace;
    test "S15.5.4.20_A5: empty string" empty_string;
    test "S15.5.4.20_A6: no whitespace" no_whitespace;
    test "S15.5.4.20_A7: Unicode whitespace" unicode_whitespace;
    test "S15.5.4.20_A8: preserves internal whitespace"
      preserves_internal_whitespace;
    test "S15.5.4.20_A9: Unicode content" unicode_content;
  ]
