(* TC39 Test262: String.prototype.indexOf tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/indexOf

   ECMA-262 Section: String.prototype.indexOf(searchString, position)

   Tests for String.Prototype.index_of *)

module String = Quickjs.String

(* ===================================================================
   Basic indexOf functionality - S15.5.4.7
   =================================================================== *)

let basic_search () =
  assert_int (String.Prototype.index_of "world" "hello world") 6;
  assert_int (String.Prototype.index_of "hello" "hello world") 0;
  assert_int (String.Prototype.index_of "o" "hello world") 4

let not_found () =
  (* Returns -1 when not found *)
  assert_int (String.Prototype.index_of "xyz" "hello world") (-1);
  assert_int (String.Prototype.index_of "HELLO" "hello world") (-1)

let with_position () =
  (* Start searching from specified position *)
  assert_int (String.Prototype.index_of_from "o" 5 "hello world") 7;
  assert_int (String.Prototype.index_of_from "hello" 1 "hello world") (-1)

let empty_search_string () =
  (* Empty search string is found at position 0 (or at specified position) *)
  assert_int (String.Prototype.index_of "" "hello") 0;
  assert_int (String.Prototype.index_of_from "" 3 "hello") 3

let empty_string () =
  assert_int (String.Prototype.index_of "a" "") (-1);
  assert_int (String.Prototype.index_of "" "") 0

let position_bounds () =
  (* Negative position treated as 0 *)
  assert_int (String.Prototype.index_of_from "h" (-10) "hello") 0;
  (* Position beyond string length returns -1 (or string length for empty search) *)
  assert_int (String.Prototype.index_of_from "h" 100 "hello") (-1)

let case_sensitive () =
  assert_int (String.Prototype.index_of "Hello" "hello world") (-1);
  assert_int (String.Prototype.index_of "hello" "Hello World") (-1)

let multiple_occurrences () =
  (* Returns first occurrence *)
  assert_int (String.Prototype.index_of "o" "hello world hello") 4;
  assert_int (String.Prototype.index_of "hello" "hello world hello") 0

let unicode_search () =
  assert_int (String.Prototype.index_of "日" "日本語") 0;
  assert_int (String.Prototype.index_of "語" "日本語") 2;
  assert_int (String.Prototype.index_of "café" "I love café") 7

let unicode_surrogate_pairs () =
  (* Emoji search - returns UTF-16 index *)
  let s = "a😀b😀c" in
  assert_int (String.Prototype.index_of "😀" s) 1;
  assert_int (String.Prototype.index_of "b" s) 3;
  assert_int (String.Prototype.index_of "c" s) 6

let tests =
  [
    test "S15.5.4.7_A1: basic search" basic_search;
    test "S15.5.4.7_A2: not found" not_found;
    test "S15.5.4.7_A3: with position" with_position;
    test "S15.5.4.7_A4: empty search string" empty_search_string;
    test "S15.5.4.7_A5: empty string" empty_string;
    test "S15.5.4.7_A6: position bounds" position_bounds;
    test "S15.5.4.7_A7: case sensitive" case_sensitive;
    test "S15.5.4.7_A8: multiple occurrences" multiple_occurrences;
    test "S15.5.4.7_A9: Unicode search" unicode_search;
    test "S15.5.4.7_A10: Unicode surrogate pairs" unicode_surrogate_pairs;
  ]
