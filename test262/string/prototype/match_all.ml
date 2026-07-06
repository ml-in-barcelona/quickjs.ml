(* TC39 Test262: String.prototype.matchAll tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/matchAll

   ECMA-262 Section: String.prototype.matchAll(regexp)

   Tests for String.Prototype.match_all *)

module String = Quickjs.String
module RegExp = Quickjs.RegExp

let full_match (m : RegExp.match_result) =
  match m.RegExp.captures.(0) with Some s -> s | None -> ""

(* ===================================================================
   Basic matchAll functionality
   =================================================================== *)

let basic_match_all () =
  let results = String.Prototype.match_all "[0-9]+" "a1b22c333" in
  assert_int (List.length results) 3;
  match results with
  | [ m1; m2; m3 ] ->
      assert_string (full_match m1) "1";
      assert_string (full_match m2) "22";
      assert_string (full_match m3) "333"
  | _ -> Alcotest.fail "Expected 3 matches"

let no_matches () =
  let results = String.Prototype.match_all "[0-9]+" "abc" in
  assert_int (List.length results) 0

let with_capture_groups () =
  let results = String.Prototype.match_all "(\\w+)@(\\w+)" "a@b and c@d" in
  assert_int (List.length results) 2;
  match results with
  | [ m1; m2 ] ->
      assert_string (full_match m1) "a@b";
      assert_int (Array.length m1.RegExp.captures) 3;
      assert_string_opt m1.RegExp.captures.(1) (Some "a");
      assert_string_opt m1.RegExp.captures.(2) (Some "b");
      assert_string (full_match m2) "c@d"
  | _ -> Alcotest.fail "Expected 2 matches"

let with_named_groups () =
  let results =
    String.Prototype.match_all "(?<user>\\w+)@(?<domain>\\w+)" "a@b c@d"
  in
  assert_int (List.length results) 2;
  match results with
  | m1 :: _ -> assert_string_opt (RegExp.group "user" m1) (Some "a")
  | _ -> Alcotest.fail "Expected matches"

let unicode_match_all () =
  let results = String.Prototype.match_all "日+" "日本日日語" in
  assert_int (List.length results) 2

let match_indices () =
  let results = String.Prototype.match_all "o" "hello world" in
  assert_int (List.length results) 2;
  match results with
  | [ m1; m2 ] ->
      assert_int m1.RegExp.index 4;
      assert_int m2.RegExp.index 7
  | _ -> Alcotest.fail "Expected 2 matches"

let match_indices_unicode () =
  (* Indices are UTF-16 code units, consistent with String.Prototype *)
  let results = String.Prototype.match_all "b" "ébé b" in
  assert_int (List.length results) 2;
  match results with
  | [ m1; m2 ] ->
      assert_int m1.RegExp.index 1;
      assert_int m2.RegExp.index 4
  | _ -> Alcotest.fail "Expected 2 matches"

let empty_matches () =
  (* Patterns that can match empty strings *)
  let results = String.Prototype.match_all "a*" "aaa" in
  (* Should match "aaa" and then empty strings at boundaries *)
  assert_bool (List.length results >= 1) true

let zero_width_group_terminates () =
  (* Regression: a zero-width capture group at the match start used to
     corrupt lastIndex and loop forever *)
  let results = String.Prototype.match_all "(x?)ab" "abab" in
  assert_int (List.length results) 2

let tests =
  [
    test "matchAll: basic" basic_match_all;
    test "matchAll: no matches" no_matches;
    test "matchAll: with capture groups" with_capture_groups;
    test "matchAll: with named groups" with_named_groups;
    test "matchAll: Unicode" unicode_match_all;
    test "matchAll: match indices" match_indices;
    test "matchAll: match indices are UTF-16" match_indices_unicode;
    test "matchAll: empty matches" empty_matches;
    test "matchAll: zero-width group terminates" zero_width_group_terminates;
  ]
