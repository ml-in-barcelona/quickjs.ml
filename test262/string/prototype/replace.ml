(* TC39 Test262: String.prototype.replace tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/replace

   ECMA-262 Section: String.prototype.replace(searchValue, replaceValue)

   Tests for String.Prototype.replace *)

module String = Quickjs.String

(* ===================================================================
   Basic replace functionality - S15.5.4.11
   =================================================================== *)

let string_replacement () =
  assert_string (String.Prototype.replace "world" "OCaml" "hello world") "hello OCaml";
  assert_string (String.Prototype.replace "o" "0" "hello world") "hell0 world"
(* Only replaces first occurrence *)

let regex_replacement () =
  assert_string (String.Prototype.replace_regex "[0-9]+" "X" "a1b2c3") "aXb2c3"
(* Without global flag, only first match *)

let regex_global_replacement () =
  assert_string (String.Prototype.replace_regex_global "[0-9]+" "X" "a1b2c3") "aXbXcX"

let no_match () =
  assert_string (String.Prototype.replace "xyz" "abc" "hello") "hello"

let empty_replacement () =
  assert_string (String.Prototype.replace "world" "" "hello world") "hello "

let empty_search () =
  (* Empty search matches at beginning *)
  assert_string (String.Prototype.replace "" "X" "hello") "Xhello"

let special_replacement_patterns () =
  (* $$ = literal $ *)
  assert_string (String.Prototype.replace "x" "$$" "axa") "a$a";
  (* $& = matched substring *)
  assert_string (String.Prototype.replace "world" "[$&]" "hello world") "hello [world]";
  (* $` = portion before match *)
  assert_string (String.Prototype.replace "world" "$`" "hello world") "hello hello ";
  (* $' = portion after match *)
  assert_string (String.Prototype.replace "hello" "$'" "hello world") " world world"

let capture_group_replacement () =
  (* $1, $2, etc. = captured groups *)
  assert_string
    (String.Prototype.replace_regex "(\\d{4})-(\\d{2})-(\\d{2})" "$2/$3/$1" "2024-07-17")
    "07/17/2024"

let unicode_replacement () =
  assert_string (String.Prototype.replace "日本" "Japan" "日本語") "Japan語";
  assert_string (String.Prototype.replace "😀" "smile" "hello 😀") "hello smile"

let case_sensitive () =
  assert_string (String.Prototype.replace "HELLO" "hi" "hello world") "hello world"

let case_insensitive_regex () =
  assert_string (String.Prototype.replace_regex_flags "hello" "i" "hi" "HELLO world") "hi world"

let tests =
  [
    test "S15.5.4.11_A1: string replacement" string_replacement;
    test "S15.5.4.11_A2: regex replacement" regex_replacement;
    test "S15.5.4.11_A3: regex global replacement" regex_global_replacement;
    test "S15.5.4.11_A4: no match" no_match;
    test "S15.5.4.11_A5: empty replacement" empty_replacement;
    test "S15.5.4.11_A6: empty search" empty_search;
    test "S15.5.4.11_A7: special replacement patterns" special_replacement_patterns;
    test "S15.5.4.11_A8: capture group replacement" capture_group_replacement;
    test "S15.5.4.11_A9: Unicode replacement" unicode_replacement;
    test "S15.5.4.11_A10: case sensitive" case_sensitive;
    test "S15.5.4.11_A11: case insensitive regex" case_insensitive_regex;
  ]

