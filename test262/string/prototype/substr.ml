(* TC39 Test262: String.prototype.substr tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/annexB/built-ins/String/prototype/substr

   ECMA-262 Annex B: String.prototype.substr(start, length)

   Note: substr is a legacy method in Annex B, but still widely used.

   Tests for String.Prototype.substr *)

module String = Quickjs.String

(* ===================================================================
   Basic substr functionality - B.2.3
   =================================================================== *)

let basic_substr () =
  assert_string
    (String.Prototype.substr ~start:0 ~length:5 "hello world")
    "hello";
  assert_string
    (String.Prototype.substr ~start:6 ~length:5 "hello world")
    "world"

let from_start_only () =
  (* When length is omitted, extract to end of string *)
  assert_string (String.Prototype.substr_from 6 "hello world") "world";
  assert_string (String.Prototype.substr_from 0 "hello") "hello"

let negative_start () =
  (* Negative start counts from end *)
  assert_string
    (String.Prototype.substr ~start:(-5) ~length:5 "hello world")
    "world";
  assert_string
    (String.Prototype.substr ~start:(-11) ~length:5 "hello world")
    "hello"

let zero_length () =
  assert_string (String.Prototype.substr ~start:0 ~length:0 "hello") ""

let negative_length () =
  (* Negative length is treated as 0 *)
  assert_string (String.Prototype.substr ~start:0 ~length:(-1) "hello") ""

let empty_string () =
  assert_string (String.Prototype.substr ~start:0 ~length:0 "") "";
  assert_string (String.Prototype.substr_from 0 "") ""

let length_exceeds_string () =
  (* Length is clamped to remaining string length *)
  assert_string (String.Prototype.substr ~start:0 ~length:100 "hello") "hello";
  assert_string (String.Prototype.substr ~start:3 ~length:100 "hello") "lo"

let start_exceeds_string () =
  assert_string (String.Prototype.substr ~start:100 ~length:5 "hello") ""

let unicode_handling () =
  assert_string (String.Prototype.substr ~start:0 ~length:2 "日本語") "日本";
  let s = "a😀b" in
  assert_string (String.Prototype.substr ~start:0 ~length:1 s) "a"

let tests =
  [
    test "B.2.3_A1: basic substr" basic_substr;
    test "B.2.3_A2: from start only" from_start_only;
    test "B.2.3_A3: negative start" negative_start;
    test "B.2.3_A4: zero length" zero_length;
    test "B.2.3_A5: negative length" negative_length;
    test "B.2.3_A6: empty string" empty_string;
    test "B.2.3_A7: length exceeds string" length_exceeds_string;
    test "B.2.3_A8: start exceeds string" start_exceeds_string;
    test "B.2.3_A9: Unicode handling" unicode_handling;
  ]
