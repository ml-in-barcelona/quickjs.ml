(* TC39 Test262: String.prototype.slice tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/slice

   ECMA-262 Section: String.prototype.slice(start, end)

   Tests for String.Prototype.slice *)

module String = Quickjs.String

(* ===================================================================
   Basic slice functionality - S15.5.4.13
   =================================================================== *)

let basic_slice () =
  assert_string (String.Prototype.slice ~start:0 ~end_:5 "hello world") "hello";
  assert_string (String.Prototype.slice ~start:6 ~end_:11 "hello world") "world";
  assert_string
    (String.Prototype.slice ~start:0 ~end_:11 "hello world")
    "hello world"

let from_start_only () =
  (* When end is omitted, slice to end of string *)
  assert_string (String.Prototype.slice_from 6 "hello world") "world";
  assert_string (String.Prototype.slice_from 0 "hello") "hello"

let negative_indices () =
  (* Negative indices count from end *)
  assert_string
    (String.Prototype.slice ~start:(-5) ~end_:11 "hello world")
    "world";
  assert_string
    (String.Prototype.slice ~start:0 ~end_:(-6) "hello world")
    "hello";
  assert_string
    (String.Prototype.slice ~start:(-5) ~end_:(-1) "hello world")
    "worl"

let empty_result () =
  (* When start >= end, result is empty *)
  assert_string (String.Prototype.slice ~start:5 ~end_:0 "hello") "";
  assert_string (String.Prototype.slice ~start:5 ~end_:5 "hello") ""

let empty_string () =
  assert_string (String.Prototype.slice ~start:0 ~end_:0 "") "";
  assert_string (String.Prototype.slice_from 0 "") ""

let out_of_bounds () =
  (* Indices are clamped to string bounds *)
  assert_string (String.Prototype.slice ~start:0 ~end_:100 "hello") "hello";
  assert_string (String.Prototype.slice ~start:(-100) ~end_:5 "hello") "hello"

let unicode_bmp () =
  (* Slice with BMP Unicode characters *)
  assert_string (String.Prototype.slice ~start:0 ~end_:4 "café") "café";
  assert_string (String.Prototype.slice ~start:0 ~end_:2 "日本語") "日本"

let unicode_surrogate_pairs () =
  (* With surrogate pairs, slice operates on UTF-16 code units *)
  let s = "a😀b" in
  (* 'a' at 0, emoji at 1-2, 'b' at 3 *)
  assert_string (String.Prototype.slice ~start:0 ~end_:1 s) "a";
  assert_string (String.Prototype.slice ~start:3 ~end_:4 s) "b"
(* Slicing in middle of surrogate pair may produce invalid UTF-8 *)

let tests =
  [
    test "S15.5.4.13_A1: basic slice" basic_slice;
    test "S15.5.4.13_A2: from start only" from_start_only;
    test "S15.5.4.13_A3: negative indices" negative_indices;
    test "S15.5.4.13_A4: empty result" empty_result;
    test "S15.5.4.13_A5: empty string" empty_string;
    test "S15.5.4.13_A6: out of bounds" out_of_bounds;
    test "S15.5.4.13_A7: Unicode BMP" unicode_bmp;
    test "S15.5.4.13_A8: Unicode surrogate pairs" unicode_surrogate_pairs;
  ]
