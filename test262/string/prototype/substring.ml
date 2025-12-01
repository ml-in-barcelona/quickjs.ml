(* TC39 Test262: String.prototype.substring tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/substring

   ECMA-262 Section: String.prototype.substring(start, end)

   Tests for String.Prototype.substring *)

module String = Quickjs.String

(* ===================================================================
   Basic substring functionality - S15.5.4.15
   =================================================================== *)

let basic_substring () =
  assert_string (String.Prototype.substring ~start:0 ~end_:5 "hello world") "hello";
  assert_string (String.Prototype.substring ~start:6 ~end_:11 "hello world") "world"

let from_start_only () =
  assert_string (String.Prototype.substring_from 6 "hello world") "world";
  assert_string (String.Prototype.substring_from 0 "hello") "hello"

let swapped_arguments () =
  (* Unlike slice, substring swaps start/end if start > end *)
  assert_string (String.Prototype.substring ~start:5 ~end_:0 "hello") "hello";
  assert_string (String.Prototype.substring ~start:11 ~end_:6 "hello world") "world"

let negative_indices () =
  (* Negative indices are treated as 0 *)
  assert_string (String.Prototype.substring ~start:(-5) ~end_:5 "hello world") "hello";
  assert_string (String.Prototype.substring ~start:0 ~end_:(-1) "hello") ""

let nan_indices () =
  (* NaN would be treated as 0 - we test boundary behavior *)
  assert_string (String.Prototype.substring ~start:0 ~end_:0 "hello") ""

let empty_string () =
  assert_string (String.Prototype.substring ~start:0 ~end_:0 "") "";
  assert_string (String.Prototype.substring_from 0 "") ""

let out_of_bounds () =
  (* Indices are clamped to string length *)
  assert_string (String.Prototype.substring ~start:0 ~end_:100 "hello") "hello";
  assert_string (String.Prototype.substring ~start:100 ~end_:200 "hello") ""

let unicode_handling () =
  (* Works on UTF-16 code units *)
  assert_string (String.Prototype.substring ~start:0 ~end_:2 "日本語") "日本";
  let s = "a😀b" in
  assert_string (String.Prototype.substring ~start:0 ~end_:1 s) "a"

let tests =
  [
    test "S15.5.4.15_A1: basic substring" basic_substring;
    test "S15.5.4.15_A2: from start only" from_start_only;
    test "S15.5.4.15_A3: swapped arguments" swapped_arguments;
    test "S15.5.4.15_A4: negative indices" negative_indices;
    test "S15.5.4.15_A5: NaN-like indices" nan_indices;
    test "S15.5.4.15_A6: empty string" empty_string;
    test "S15.5.4.15_A7: out of bounds" out_of_bounds;
    test "S15.5.4.15_A8: Unicode handling" unicode_handling;
  ]

