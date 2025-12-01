(* TC39 Test262: String.prototype.includes tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/includes

   ECMA-262 Section: String.prototype.includes(searchString, position)

   Tests for String.Prototype.includes *)

module String = Quickjs.String

(* ===================================================================
   Basic includes functionality
   =================================================================== *)

let basic_includes () =
  assert_bool (String.Prototype.includes "world" "hello world") true;
  assert_bool (String.Prototype.includes "hello" "hello world") true;
  assert_bool (String.Prototype.includes "xyz" "hello world") false

let with_position () =
  assert_bool (String.Prototype.includes_from "world" 0 "hello world") true;
  assert_bool (String.Prototype.includes_from "hello" 1 "hello world") false;
  assert_bool (String.Prototype.includes_from "world" 6 "hello world") true

let empty_search_string () =
  (* Empty string is always included *)
  assert_bool (String.Prototype.includes "" "hello") true;
  assert_bool (String.Prototype.includes "" "") true

let empty_string () = assert_bool (String.Prototype.includes "a" "") false

let case_sensitive () =
  assert_bool (String.Prototype.includes "Hello" "hello world") false;
  assert_bool (String.Prototype.includes "WORLD" "hello world") false

let unicode_includes () =
  assert_bool (String.Prototype.includes "日本" "日本語") true;
  assert_bool (String.Prototype.includes "café" "I love café") true;
  assert_bool (String.Prototype.includes "😀" "hello 😀 world") true

let position_bounds () =
  (* Negative position treated as 0 *)
  assert_bool (String.Prototype.includes_from "hello" (-10) "hello world") true;
  (* Position beyond string returns false *)
  assert_bool (String.Prototype.includes_from "hello" 100 "hello world") false

let tests =
  [
    test "includes: basic" basic_includes;
    test "includes: with position" with_position;
    test "includes: empty search string" empty_search_string;
    test "includes: empty string" empty_string;
    test "includes: case sensitive" case_sensitive;
    test "includes: Unicode" unicode_includes;
    test "includes: position bounds" position_bounds;
  ]
