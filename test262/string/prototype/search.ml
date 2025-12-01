(* TC39 Test262: String.prototype.search tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/search

   ECMA-262 Section: String.prototype.search(regexp)

   Tests for String.Prototype.search *)

module String = Quickjs.String

(* ===================================================================
   Basic search functionality - S15.5.4.12
   =================================================================== *)

let basic_search () =
  assert_int (String.Prototype.search "[0-9]+" "abc123def") 3;
  assert_int (String.Prototype.search "world" "hello world") 6

let no_match () =
  assert_int (String.Prototype.search "[0-9]+" "abcdef") (-1);
  assert_int (String.Prototype.search "xyz" "hello world") (-1)

let search_at_start () =
  assert_int (String.Prototype.search "hello" "hello world") 0;
  assert_int (String.Prototype.search "^hello" "hello world") 0

let search_at_end () =
  assert_int (String.Prototype.search "world$" "hello world") 6

let case_sensitive () =
  assert_int (String.Prototype.search "HELLO" "hello world") (-1)

let case_insensitive () =
  assert_int (String.Prototype.search_flags "hello" "i" "HELLO world") 0

let empty_pattern () =
  (* Empty pattern matches at position 0 *)
  assert_int (String.Prototype.search "" "hello") 0

let empty_string () =
  assert_int (String.Prototype.search "" "") 0;
  assert_int (String.Prototype.search "a" "") (-1)

let unicode_search () =
  assert_int (String.Prototype.search "日" "hello日本") 5;
  assert_int (String.Prototype.search "😀" "hello 😀 world") 6

let special_regex_chars () =
  (* Characters that need escaping in regex *)
  assert_int (String.Prototype.search "\\." "hello.world") 5;
  assert_int (String.Prototype.search "\\$" "price: $100") 7

let multiline () =
  let s = "hello\nworld" in
  assert_int (String.Prototype.search "^world" s) (-1);
  assert_int (String.Prototype.search_flags "^world" "m" s) 6

let tests =
  [
    test "S15.5.4.12_A1: basic search" basic_search;
    test "S15.5.4.12_A2: no match" no_match;
    test "S15.5.4.12_A3: search at start" search_at_start;
    test "S15.5.4.12_A4: search at end" search_at_end;
    test "S15.5.4.12_A5: case sensitive" case_sensitive;
    test "S15.5.4.12_A6: case insensitive" case_insensitive;
    test "S15.5.4.12_A7: empty pattern" empty_pattern;
    test "S15.5.4.12_A8: empty string" empty_string;
    test "S15.5.4.12_A9: Unicode search" unicode_search;
    test "S15.5.4.12_A10: special regex chars" special_regex_chars;
    test "S15.5.4.12_A11: multiline" multiline;
  ]
