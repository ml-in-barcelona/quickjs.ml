(* TC39 Test262: String.prototype.replaceAll tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/replaceAll

   ECMA-262 Section: String.prototype.replaceAll(searchValue, replaceValue)

   Tests for String.Prototype.replace_all *)

module String = Quickjs.String

(* ===================================================================
   Basic replaceAll functionality
   =================================================================== *)

let string_replace_all () =
  assert_string
    (String.Prototype.replace_all "o" "0" "hello world")
    "hell0 w0rld";
  assert_string (String.Prototype.replace_all "l" "L" "hello") "heLLo"

let no_match () =
  assert_string (String.Prototype.replace_all "xyz" "abc" "hello") "hello"

let empty_replacement () =
  assert_string (String.Prototype.replace_all "o" "" "hello world") "hell wrld"

let empty_search () =
  (* Empty search inserts between every character and at start/end *)
  assert_string (String.Prototype.replace_all "" "-" "ab") "-a-b-"

let special_replacement_patterns () =
  assert_string (String.Prototype.replace_all "x" "$$" "xax") "$a$";
  assert_string (String.Prototype.replace_all "o" "[$&]" "hello") "hell[o]"

let unicode_replace_all () =
  assert_string (String.Prototype.replace_all "日" "SUN" "日日本") "SUNSUN本";
  assert_string (String.Prototype.replace_all "😀" ":)" "😀 hi 😀") ":) hi :)"

let case_sensitive () =
  assert_string (String.Prototype.replace_all "A" "X" "aAaA") "aXaX"

let overlapping_matches () =
  (* replaceAll doesn't find overlapping matches *)
  assert_string (String.Prototype.replace_all "aa" "X" "aaaa") "XX"

let regex_replace_all () =
  (* With regex (must have global flag) *)
  assert_string
    (String.Prototype.replace_all_regex "[0-9]" "X" "a1b2c3")
    "aXbXcX"

let tests =
  [
    test "replaceAll: string" string_replace_all;
    test "replaceAll: no match" no_match;
    test "replaceAll: empty replacement" empty_replacement;
    test "replaceAll: empty search" empty_search;
    test "replaceAll: special patterns" special_replacement_patterns;
    test "replaceAll: Unicode" unicode_replace_all;
    test "replaceAll: case sensitive" case_sensitive;
    test "replaceAll: overlapping matches" overlapping_matches;
    test "replaceAll: regex" regex_replace_all;
  ]
