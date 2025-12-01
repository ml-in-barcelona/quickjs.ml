(* TC39 Test262: String.prototype.startsWith tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/startsWith

   ECMA-262 Section: String.prototype.startsWith(searchString, position)

   Tests for String.Prototype.starts_with *)

module String = Quickjs.String

(* ===================================================================
   Basic startsWith functionality
   =================================================================== *)

let basic_starts_with () =
  assert_bool (String.Prototype.starts_with "hello" "hello world") true;
  assert_bool (String.Prototype.starts_with "world" "hello world") false;
  assert_bool (String.Prototype.starts_with "" "hello") true

let with_position () =
  (* Check if string starts with search at given position *)
  assert_bool (String.Prototype.starts_with_from "world" 6 "hello world") true;
  assert_bool (String.Prototype.starts_with_from "hello" 0 "hello world") true;
  assert_bool (String.Prototype.starts_with_from "hello" 1 "hello world") false

let empty_search_string () =
  assert_bool (String.Prototype.starts_with "" "hello") true;
  assert_bool (String.Prototype.starts_with_from "" 3 "hello") true

let empty_string () =
  assert_bool (String.Prototype.starts_with "" "") true;
  assert_bool (String.Prototype.starts_with "a" "") false

let case_sensitive () =
  assert_bool (String.Prototype.starts_with "Hello" "hello world") false;
  assert_bool (String.Prototype.starts_with "HELLO" "hello world") false

let unicode_starts_with () =
  assert_bool (String.Prototype.starts_with "日本" "日本語") true;
  assert_bool (String.Prototype.starts_with "😀" "😀 hello") true;
  assert_bool (String.Prototype.starts_with "café" "café au lait") true

let position_bounds () =
  assert_bool (String.Prototype.starts_with_from "hello" (-10) "hello world") true;
  assert_bool (String.Prototype.starts_with_from "hello" 100 "hello world") false

let search_longer_than_string () =
  assert_bool (String.Prototype.starts_with "hello world!" "hello") false

let tests =
  [
    test "startsWith: basic" basic_starts_with;
    test "startsWith: with position" with_position;
    test "startsWith: empty search string" empty_search_string;
    test "startsWith: empty string" empty_string;
    test "startsWith: case sensitive" case_sensitive;
    test "startsWith: Unicode" unicode_starts_with;
    test "startsWith: position bounds" position_bounds;
    test "startsWith: search longer than string" search_longer_than_string;
  ]

