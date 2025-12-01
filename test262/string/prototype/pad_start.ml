(* TC39 Test262: String.prototype.padStart tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/padStart

   ECMA-262 Section: String.prototype.padStart(maxLength, fillString)

   Tests for String.Prototype.pad_start *)

module String = Quickjs.String

(* ===================================================================
   Basic padStart functionality
   =================================================================== *)

let basic_pad_start () =
  assert_string (String.Prototype.pad_start 5 "abc") "  abc";
  assert_string (String.Prototype.pad_start 8 "hello") "   hello"

let with_fill_string () =
  assert_string (String.Prototype.pad_start_with 5 "0" "42") "00042";
  assert_string (String.Prototype.pad_start_with 10 "foo" "bar") "foofoofbar"

let length_less_than_string () =
  (* If maxLength <= string length, return original string *)
  assert_string (String.Prototype.pad_start 3 "hello") "hello";
  assert_string (String.Prototype.pad_start 0 "hello") "hello"

let empty_fill_string () =
  (* Empty fill string returns original *)
  assert_string (String.Prototype.pad_start_with 10 "" "hello") "hello"

let empty_string () =
  assert_string (String.Prototype.pad_start 5 "") "     ";
  assert_string (String.Prototype.pad_start_with 5 "ab" "") "ababa"

let fill_string_truncation () =
  (* Fill string is truncated to fit exactly *)
  assert_string (String.Prototype.pad_start_with 7 "abc" "x") "abcabcx"

let unicode_string () =
  (* padStart works on UTF-16 code units *)
  assert_string (String.Prototype.pad_start 5 "日本") "   日本"

let unicode_fill () =
  assert_string (String.Prototype.pad_start_with 5 "日" "ab") "日日日ab"

let surrogate_pairs () =
  (* Emoji takes 2 UTF-16 code units *)
  let padded = String.Prototype.pad_start 5 "😀" in
  (* 😀 is 2 code units, need 3 more spaces to reach 5 *)
  assert_int (String.Prototype.length padded) 5

let negative_length () =
  (* Negative maxLength returns original *)
  assert_string (String.Prototype.pad_start (-5) "hello") "hello"

let tests =
  [
    test "padStart: basic" basic_pad_start;
    test "padStart: with fill string" with_fill_string;
    test "padStart: length less than string" length_less_than_string;
    test "padStart: empty fill string" empty_fill_string;
    test "padStart: empty string" empty_string;
    test "padStart: fill string truncation" fill_string_truncation;
    test "padStart: Unicode string" unicode_string;
    test "padStart: Unicode fill" unicode_fill;
    test "padStart: surrogate pairs" surrogate_pairs;
    test "padStart: negative length" negative_length;
  ]

