(** Extended utilities for Alcotest used in TC39 Test262 tests *)

module RegExp = Quickjs.RegExp
module Number = Quickjs.Number

(** Expected failures - tests that we know don't pass yet but want to track *)
let expected_failures : string list = [
  (* Add test IDs here as we discover failures *)
  (* Example: "parseInt.S15.1.2.2_A2_T10_U180E" for Mongolian vowel separator *)
]

(** Check if a test is expected to fail *)
let is_expected_failure test_id =
  List.mem test_id expected_failures

(** Create a test case that handles expected failures *)
let test ?(expected_fail = false) title fn =
  if expected_fail then
    Alcotest.test_case (title ^ " [EXPECTED FAIL]") `Quick (fun () ->
      try
        fn ();
        Alcotest.fail "Expected failure but test passed - remove from expected_failures!"
      with _ -> ())
  else
    Alcotest.test_case title `Quick fn

(** Create a test case by ID, auto-detecting expected failures *)
let test_by_id id title fn =
  test ~expected_fail:(is_expected_failure id) title fn

(* ===== Assertion helpers ===== *)

let assert_float left right =
  Alcotest.(check (float 0.0001)) "should be equal" right left

let assert_float_exact left right =
  Alcotest.(check (float 0.0)) "should be exactly equal" right left

let assert_int left right =
  Alcotest.(check int) "should be equal" right left

let assert_string left right =
  Alcotest.(check string) "should be equal" right left

let assert_bool left right =
  Alcotest.(check bool) "should be equal" right left

let assert_nan value =
  Alcotest.(check bool) "should be NaN" true (Float.is_nan value)

let assert_not_nan value =
  Alcotest.(check bool) "should not be NaN" false (Float.is_nan value)

let assert_infinity value =
  Alcotest.(check bool) "should be Infinity" true (value = infinity)

let assert_neg_infinity value =
  Alcotest.(check bool) "should be -Infinity" true (value = neg_infinity)

let assert_positive_zero value =
  Alcotest.(check bool) "should be +0" true (value = 0.0 && (1.0 /. value) = infinity)

let assert_negative_zero value =
  Alcotest.(check bool) "should be -0" true (value = 0.0 && (1.0 /. value) = neg_infinity)

let assert_array left right =
  Alcotest.(check (array string)) "should be equal" right left

(* ===== RegExp helpers ===== *)

let regexp_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok regexp -> regexp
  | Error (_, error) ->
      Alcotest.fail
        (Printf.sprintf
           "This regex should not fail to compile, it failed with %s" error)

let regexp_no_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok _regexp -> Alcotest.fail "This regex should fail to compile, it succeeded"
  | Error (_, error) -> error

(* ===== JavaScript compatibility constants ===== *)

(** Maximum safe integer in JavaScript: 2^53 - 1 *)
let max_safe_integer = 9007199254740991.0

(** Minimum safe integer in JavaScript: -(2^53 - 1) *)
let min_safe_integer = -9007199254740991.0

(** Maximum value in JavaScript *)
let max_value = 1.7976931348623157e+308

(** Minimum positive value in JavaScript *)
let min_value = 5e-324

(** Epsilon in JavaScript *)
let epsilon = 2.220446049250313e-16

