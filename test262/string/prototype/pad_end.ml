(* TC39 Test262: String.prototype.padEnd tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/padEnd

   ECMA-262 Section: String.prototype.padEnd(maxLength, fillString)

   Tests for String.Prototype.pad_end *)

module String = Quickjs.String

(* ===================================================================
   Basic padEnd functionality
   =================================================================== *)

let basic_pad_end () =
  assert_string (String.Prototype.pad_end 5 "abc") "abc  ";
  assert_string (String.Prototype.pad_end 8 "hello") "hello   "

let with_fill_string () =
  assert_string (String.Prototype.pad_end_with 5 "0" "42") "42000";
  assert_string (String.Prototype.pad_end_with 10 "foo" "bar") "barfoofoof"

let length_less_than_string () =
  assert_string (String.Prototype.pad_end 3 "hello") "hello";
  assert_string (String.Prototype.pad_end 0 "hello") "hello"

let empty_fill_string () =
  assert_string (String.Prototype.pad_end_with 10 "" "hello") "hello"

let empty_string () =
  assert_string (String.Prototype.pad_end 5 "") "     ";
  assert_string (String.Prototype.pad_end_with 5 "ab" "") "ababa"

let fill_string_truncation () =
  assert_string (String.Prototype.pad_end_with 7 "abc" "x") "xabcabc"

let unicode_string () = assert_string (String.Prototype.pad_end 5 "日本") "日本   "

let unicode_fill () =
  assert_string (String.Prototype.pad_end_with 5 "日" "ab") "ab日日日"

let surrogate_pairs () =
  let padded = String.Prototype.pad_end 5 "😀" in
  assert_int (String.Prototype.length padded) 5

let negative_length () =
  assert_string (String.Prototype.pad_end (-5) "hello") "hello"

let tests =
  [
    test "padEnd: basic" basic_pad_end;
    test "padEnd: with fill string" with_fill_string;
    test "padEnd: length less than string" length_less_than_string;
    test "padEnd: empty fill string" empty_fill_string;
    test "padEnd: empty string" empty_string;
    test "padEnd: fill string truncation" fill_string_truncation;
    test "padEnd: Unicode string" unicode_string;
    test "padEnd: Unicode fill" unicode_fill;
    test "padEnd: surrogate pairs" surrogate_pairs;
    test "padEnd: negative length" negative_length;
  ]
