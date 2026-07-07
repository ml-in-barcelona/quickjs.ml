(** Extended utilities for Alcotest used in TC39 Test262 tests *)

module RegExp = Quickjs.RegExp

let test title fn = Alcotest.test_case title `Quick fn

(* ===== Assertion helpers ===== *)

let assert_float left right =
  Alcotest.(check (float 0.0001)) "should be equal" right left

let assert_float_exact left right =
  Alcotest.(check (float 0.0)) "should be exactly equal" right left

let assert_int left right = Alcotest.(check int) "should be equal" right left

let assert_string left right =
  Alcotest.(check string) "should be equal" right left

let assert_bool left right = Alcotest.(check bool) "should be equal" right left

let assert_nan value =
  Alcotest.(check bool) "should be NaN" true (Float.is_nan value)

let assert_not_nan value =
  Alcotest.(check bool) "should not be NaN" false (Float.is_nan value)

let assert_infinity value =
  Alcotest.(check bool) "should be Infinity" true (value = infinity)

let assert_neg_infinity value =
  Alcotest.(check bool) "should be -Infinity" true (value = neg_infinity)

let assert_positive_zero value =
  Alcotest.(check bool)
    "should be +0" true
    (value = 0.0 && 1.0 /. value = infinity)

let assert_negative_zero value =
  Alcotest.(check bool)
    "should be -0" true
    (value = 0.0 && 1.0 /. value = neg_infinity)

let assert_array left right =
  Alcotest.(check (array string)) "should be equal" right left

let assert_option_array left right =
  Alcotest.(check (array (option string))) "should be equal" right left

(* ===== RegExp helpers ===== *)

let regexp_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok regexp -> regexp
  | Error error ->
      Alcotest.fail
        (Printf.sprintf
           "This regex should not fail to compile, it failed with %s"
           (RegExp.compile_error_to_string error))

let regexp_no_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok _regexp ->
      Alcotest.fail "This regex should fail to compile, it succeeded"
  | Error error -> error

(* ===== RegExp match assertions ===== *)

(** Assert that a match happened and that all captures participated with the
    given values. Group 0 is the full match. *)
let assert_match (result : RegExp.match_result option) expected =
  match result with
  | None ->
      Alcotest.fail
        (Printf.sprintf "expected a match with %d captures, got no match"
           (Array.length expected))
  | Some m ->
      Alcotest.(check (array (option string)))
        "captures should be equal"
        (Array.map (fun c -> Some c) expected)
        m.RegExp.captures

(** Assert captures including non-participating groups (None). *)
let assert_captures (result : RegExp.match_result option) expected =
  match result with
  | None -> Alcotest.fail "expected a match, got no match"
  | Some m ->
      Alcotest.(check (array (option string)))
        "captures should be equal" expected m.RegExp.captures

let assert_no_match (result : RegExp.match_result option) =
  match result with
  | None -> ()
  | Some m ->
      Alcotest.fail
        (Printf.sprintf "expected no match, but matched %S at index %d"
           (match m.RegExp.captures.(0) with Some s -> s | None -> "")
           m.RegExp.index)

(** Assert the UTF-16 index of a match. *)
let assert_match_index (result : RegExp.match_result option) expected =
  match result with
  | None -> Alcotest.fail "expected a match, got no match"
  | Some m ->
      Alcotest.(check int) "index should be equal" expected m.RegExp.index

(** Assert the value of a named group ([None] = absent or non-participating). *)
let assert_group (result : RegExp.match_result option) name expected =
  match result with
  | None -> Alcotest.fail "expected a match, got no match"
  | Some m ->
      Alcotest.(check (option string))
        (Printf.sprintf "group %S should be equal" name)
        expected (RegExp.group name m)

(* ===== RegExp error assertions ===== *)

let assert_compile_error ~expected actual =
  let expected_str = RegExp.compile_error_to_string expected in
  let actual_str = RegExp.compile_error_to_string actual in
  Alcotest.(check string) "compile error should match" expected_str actual_str

let assert_unexpected_end error =
  match error with
  | `Unexpected_end -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "Expected `Unexpected_end but got %s"
           (RegExp.compile_error_to_string other))

let assert_nothing_to_repeat error =
  match error with
  | `Nothing_to_repeat -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "Expected `Nothing_to_repeat but got %s"
           (RegExp.compile_error_to_string other))

let assert_invalid_escape error =
  match error with
  | `Invalid_escape_sequence -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "Expected `Invalid_escape_sequence but got %s"
           (RegExp.compile_error_to_string other))

let assert_malformed_unicode error =
  match error with
  | `Malformed_unicode_char -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "Expected `Malformed_unicode_char but got %s"
           (RegExp.compile_error_to_string other))

let assert_invalid_flags error =
  match error with
  | `Invalid_flags _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "Expected `Invalid_flags but got %s"
           (RegExp.compile_error_to_string other))

let string_contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len = 0 then true
  else if needle_len > haystack_len then false
  else
    let rec check i =
      if i + needle_len > haystack_len then false
      else if String.sub haystack i needle_len = needle then true
      else check (i + 1)
    in
    check 0

let assert_unknown_error ~contains error =
  match error with
  | `Unknown msg ->
      if
        String.length contains > 0 && not (string_contains ~needle:contains msg)
      then
        Alcotest.fail
          (Printf.sprintf "Expected unknown error containing '%s' but got '%s'"
             contains msg)
  | other ->
      Alcotest.fail
        (Printf.sprintf "Expected `Unknown error but got %s"
           (RegExp.compile_error_to_string other))

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

(* ===== Option type assertions ===== *)

let assert_float_opt left right =
  Alcotest.(check (option (float 0.0))) "should be equal" right left

let assert_int_opt left right =
  Alcotest.(check (option int)) "should be equal" right left

let assert_string_opt left right =
  Alcotest.(check (option string)) "should be equal" right left

(* ===== Unicode assertions ===== *)

let assert_uchar left right =
  Alcotest.(check int)
    "Uchar should be equal" (Uchar.to_int right) (Uchar.to_int left)
