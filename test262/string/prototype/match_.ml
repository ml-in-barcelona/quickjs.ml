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
  match result with
  | Some matches ->
      assert_int (Array.length matches) 1;
      assert_string matches.(0) "123"
  | None -> Alcotest.fail "Expected match"

let no_match () =
  let result = String.Prototype.match_ "[0-9]+" "abcdef" in
  match result with Some _ -> Alcotest.fail "Expected no match" | None -> ()

let global_flag () =
  (* With global flag, returns all matches *)
  let result = String.Prototype.match_global "[0-9]+" "a1b2c3d" in
  assert_int (Array.length result) 3;
  assert_string result.(0) "1";
  assert_string result.(1) "2";
  assert_string result.(2) "3"

let capture_groups () =
  let result =
    String.Prototype.match_ "(\\d{4})-(\\d{2})-(\\d{2})" "Date: 2024-07-17"
  in
  match result with
  | Some matches ->
      assert_int (Array.length matches) 4;
      assert_string matches.(0) "2024-07-17";
      assert_string matches.(1) "2024";
      assert_string matches.(2) "07";
      assert_string matches.(3) "17"
  | None -> Alcotest.fail "Expected match"

let unicode_match () =
  let result = String.Prototype.match_ "日+" "日日本語" in
  match result with
  | Some matches ->
      assert_int (Array.length matches) 1;
      assert_string matches.(0) "日日"
  | None -> Alcotest.fail "Expected match"

let case_insensitive () =
  let result = String.Prototype.match_flags "hello" "i" "HELLO world" in
  match result with
  | Some matches -> assert_string matches.(0) "HELLO"
  | None -> Alcotest.fail "Expected match"

let empty_string () =
  (* /a*/ matches zero 'a's at the start of empty string, returning [""] *)
  let result = String.Prototype.match_ "a*" "" in
  match result with
  | Some matches -> assert_string matches.(0) ""
  | None -> Alcotest.fail "Expected match (empty string matches a*)"

let tests =
  [
    test "S15.5.4.10_A1: basic match" basic_match;
    test "S15.5.4.10_A2: no match" no_match;
    test "S15.5.4.10_A3: global flag" global_flag;
    test "S15.5.4.10_A4: capture groups" capture_groups;
    test "S15.5.4.10_A5: Unicode match" unicode_match;
    test "S15.5.4.10_A6: case insensitive" case_insensitive;
    test "S15.5.4.10_A7: empty string" empty_string;
  ]
