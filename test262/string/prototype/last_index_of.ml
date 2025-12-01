(* TC39 Test262: String.prototype.lastIndexOf tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/lastIndexOf

   ECMA-262 Section: String.prototype.lastIndexOf(searchString, position)

   Tests for String.Prototype.last_index_of *)

module String = Quickjs.String

(* ===================================================================
   Basic lastIndexOf functionality - S15.5.4.8
   =================================================================== *)

let basic_search () =
  assert_int (String.Prototype.last_index_of "o" "hello world") 7;
  assert_int (String.Prototype.last_index_of "hello" "hello world hello") 12;
  assert_int (String.Prototype.last_index_of "l" "hello world") 9

let not_found () =
  assert_int (String.Prototype.last_index_of "xyz" "hello world") (-1);
  assert_int (String.Prototype.last_index_of "HELLO" "hello world") (-1)

let with_position () =
  (* Search backwards from specified position *)
  assert_int (String.Prototype.last_index_of_from "o" 6 "hello world") 4;
  assert_int (String.Prototype.last_index_of_from "hello" 5 "hello world hello") 0

let empty_search_string () =
  (* Empty search string found at end (or at specified position) *)
  assert_int (String.Prototype.last_index_of "" "hello") 5;
  assert_int (String.Prototype.last_index_of_from "" 3 "hello") 3

let empty_string () =
  assert_int (String.Prototype.last_index_of "a" "") (-1);
  assert_int (String.Prototype.last_index_of "" "") 0

let position_bounds () =
  (* Negative position still searches the string *)
  assert_int (String.Prototype.last_index_of_from "h" (-10) "hello") (-1);
  (* Position beyond string length searches entire string *)
  assert_int (String.Prototype.last_index_of_from "o" 100 "hello world") 7

let case_sensitive () =
  assert_int (String.Prototype.last_index_of "Hello" "hello world") (-1)

let unicode_search () =
  assert_int (String.Prototype.last_index_of "日" "日本語日本") 3;
  assert_int (String.Prototype.last_index_of "café" "café au café") 8

let tests =
  [
    test "S15.5.4.8_A1: basic search" basic_search;
    test "S15.5.4.8_A2: not found" not_found;
    test "S15.5.4.8_A3: with position" with_position;
    test "S15.5.4.8_A4: empty search string" empty_search_string;
    test "S15.5.4.8_A5: empty string" empty_string;
    test "S15.5.4.8_A6: position bounds" position_bounds;
    test "S15.5.4.8_A7: case sensitive" case_sensitive;
    test "S15.5.4.8_A8: Unicode search" unicode_search;
  ]

