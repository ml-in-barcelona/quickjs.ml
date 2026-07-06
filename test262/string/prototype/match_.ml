(* TC39 Test262: String.prototype.match tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/match

   ECMA-262 Section: String.prototype.match(regexp)

   Tests for String.Prototype.match_ *)

module String = Quickjs.String
module RegExp = Quickjs.RegExp

(* ===================================================================
   Basic match functionality - S15.5.4.10
   =================================================================== *)

let basic_match () =
  let result = String.Prototype.match_ "[0-9]+" "abc123def" in
  assert_match result [| "123" |];
  assert_match_index result 3

let no_match () =
  let result = String.Prototype.match_ "[0-9]+" "abcdef" in
  assert_no_match result

let global_flag () =
  (* With global flag, returns all matches *)
  let result = String.Prototype.match_global "[0-9]+" "a1b2c3d" in
  assert_array result [| "1"; "2"; "3" |]

let global_flag_extra_flags () =
  (* match_global accepts additional flags *)
  let result = String.Prototype.match_global ~flags:"i" "[a-z]" "AbC" in
  assert_array result [| "A"; "b"; "C" |]

let match_flags_rejects_global () =
  (* match_flags cannot represent multiple matches: 'g' is rejected *)
  match String.Prototype.match_flags "a" "g" "aaa" with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "Expected Invalid_argument for 'g' in match_flags"

let capture_groups () =
  let result =
    String.Prototype.match_ "(\\d{4})-(\\d{2})-(\\d{2})" "Date: 2024-07-17"
  in
  assert_match result [| "2024-07-17"; "2024"; "07"; "17" |]

let unicode_match () =
  let result = String.Prototype.match_ "日+" "日日本語" in
  assert_match result [| "日日" |];
  (* index is in UTF-16 code units *)
  assert_match_index result 0

let case_insensitive () =
  let result = String.Prototype.match_flags "hello" "i" "HELLO world" in
  assert_match result [| "HELLO" |]

let empty_string () =
  (* /a*/ matches zero 'a's at the start of empty string, returning [""] *)
  let result = String.Prototype.match_ "a*" "" in
  assert_match result [| "" |]

let invalid_pattern () =
  match String.Prototype.match_ "(" "abc" with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "Expected Invalid_argument for invalid pattern"

let tests =
  [
    test "S15.5.4.10_A1: basic match" basic_match;
    test "S15.5.4.10_A2: no match" no_match;
    test "S15.5.4.10_A3: global flag" global_flag;
    test "S15.5.4.10_A3_T2: global with extra flags" global_flag_extra_flags;
    test "S15.5.4.10_A3_T3: match_flags rejects 'g'" match_flags_rejects_global;
    test "S15.5.4.10_A4: capture groups" capture_groups;
    test "S15.5.4.10_A5: Unicode match" unicode_match;
    test "S15.5.4.10_A6: case insensitive" case_insensitive;
    test "S15.5.4.10_A7: empty string" empty_string;
    test "S15.5.4.10_A8: invalid pattern raises" invalid_pattern;
  ]
