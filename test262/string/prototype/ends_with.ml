(* TC39 Test262: String.prototype.endsWith tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/endsWith

   ECMA-262 Section: String.prototype.endsWith(searchString, endPosition)

   Tests for String.Prototype.ends_with *)

module String = Quickjs.String

(* ===================================================================
   Basic endsWith functionality
   =================================================================== *)

let basic_ends_with () =
  assert_bool (String.Prototype.ends_with "world" "hello world") true;
  assert_bool (String.Prototype.ends_with "hello" "hello world") false;
  assert_bool (String.Prototype.ends_with "" "hello") true

let with_end_position () =
  (* Check if string (up to endPosition) ends with search *)
  assert_bool (String.Prototype.ends_with_at "hello" 5 "hello world") true;
  assert_bool (String.Prototype.ends_with_at "world" 11 "hello world") true;
  assert_bool (String.Prototype.ends_with_at "lo" 5 "hello world") true

let empty_search_string () =
  assert_bool (String.Prototype.ends_with "" "hello") true;
  assert_bool (String.Prototype.ends_with_at "" 3 "hello") true

let empty_string () =
  assert_bool (String.Prototype.ends_with "" "") true;
  assert_bool (String.Prototype.ends_with "a" "") false

let case_sensitive () =
  assert_bool (String.Prototype.ends_with "World" "hello world") false;
  assert_bool (String.Prototype.ends_with "WORLD" "hello world") false

let unicode_ends_with () =
  assert_bool (String.Prototype.ends_with "語" "日本語") true;
  assert_bool (String.Prototype.ends_with "😀" "hello 😀") true;
  assert_bool (String.Prototype.ends_with "café" "I love café") true

let position_bounds () =
  (* Negative endPosition uses 0 as length *)
  assert_bool (String.Prototype.ends_with_at "hello" (-10) "hello world") false;
  (* endPosition beyond string uses string length *)
  assert_bool (String.Prototype.ends_with_at "world" 100 "hello world") true

let search_longer_than_string () =
  assert_bool (String.Prototype.ends_with "hello world!" "world") false

let tests =
  [
    test "endsWith: basic" basic_ends_with;
    test "endsWith: with endPosition" with_end_position;
    test "endsWith: empty search string" empty_search_string;
    test "endsWith: empty string" empty_string;
    test "endsWith: case sensitive" case_sensitive;
    test "endsWith: Unicode" unicode_ends_with;
    test "endsWith: position bounds" position_bounds;
    test "endsWith: search longer than string" search_longer_than_string;
  ]

