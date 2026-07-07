(* TC39 Test262: RegExp match indices ('d' flag) tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/RegExp/match-indices

   ECMA-262: RegExp Match Indices (ES2022). With the 'd' flag, a match
   result carries the [start, end) range of every capture group, in UTF-16
   code units. Without the flag, [indices] is [None] (JavaScript's
   undefined). *)

module RegExp = Quickjs.RegExp

let exec_exn re input =
  match RegExp.exec re input with
  | Some m -> m
  | None -> Alcotest.fail "expected a match, got no match"

let indices_exn (m : RegExp.match_result) =
  match m.indices with
  | Some indices -> indices
  | None -> Alcotest.fail "expected indices (the 'd' flag is set)"

let assert_range name (actual : (int * int) option) expected =
  Alcotest.(check (option (pair int int))) name expected actual

let without_d_flag_indices_is_none () =
  let re = regexp_compile "b(c)" ~flags:"" in
  let m = exec_exn re "abcd" in
  match m.indices with
  | None -> ()
  | Some _ -> Alcotest.fail "indices must be None without the 'd' flag"

let basic_indices () =
  (* /b(c)/d.exec("abcd").indices is [[1, 3], [2, 3]] *)
  let re = regexp_compile "b(c)" ~flags:"d" in
  let m = exec_exn re "abcd" in
  let indices = indices_exn m in
  assert_int (Array.length indices.ranges) 2;
  assert_range "full match" indices.ranges.(0) (Some (1, 3));
  assert_range "group 1" indices.ranges.(1) (Some (2, 3))

let range_matches_captured_substring () =
  (* indices[i] delimits exactly the captured substring:
     slice(start, end) = captures[i] *)
  let re = regexp_compile "(\\w+) (\\w+)" ~flags:"d" in
  let m = exec_exn re "hello world" in
  let indices = indices_exn m in
  Array.iteri
    (fun i range ->
      match (range, m.captures.(i)) with
      | Some (start, end_), Some capture ->
          assert_string
            (Quickjs.String.Prototype.slice ~start ~end_ m.input)
            capture
      | None, None -> ()
      | _ -> Alcotest.fail "range and capture participation must agree")
    indices.ranges

let non_participating_group_is_none () =
  (* /(a)|(b)/d.exec("b").indices is [[0, 1], undefined, [0, 1]] *)
  let re = regexp_compile "(a)|(b)" ~flags:"d" in
  let m = exec_exn re "b" in
  let indices = indices_exn m in
  assert_range "full match" indices.ranges.(0) (Some (0, 1));
  assert_range "group 1 did not participate" indices.ranges.(1) None;
  assert_range "group 2" indices.ranges.(2) (Some (0, 1))

let named_groups_indices () =
  (* /(?<year>\d{4})-(?<month>\d{2})/d.exec("2024-06").indices.groups *)
  let re = regexp_compile "(?<year>\\d{4})-(?<month>\\d{2})" ~flags:"d" in
  let m = exec_exn re "2024-06" in
  let indices = indices_exn m in
  assert_range "full match" indices.ranges.(0) (Some (0, 7));
  (match indices.groups with
  | [ ("year", year); ("month", month) ] ->
      assert_range "year" year (Some (0, 4));
      assert_range "month" month (Some (5, 7))
  | _ -> Alcotest.fail "expected named groups [year; month] in source order");
  assert_range "group_indices year"
    (RegExp.group_indices "year" m)
    (Some (0, 4));
  assert_range "group_indices month"
    (RegExp.group_indices "month" m)
    (Some (5, 7));
  assert_range "group_indices unknown" (RegExp.group_indices "day" m) None

let group_indices_without_d_flag () =
  let re = regexp_compile "(?<year>\\d{4})" ~flags:"" in
  let m = exec_exn re "2024" in
  assert_range "no 'd' flag" (RegExp.group_indices "year" m) None

let group_indices_non_participating () =
  (* /(?<a>x)|(?<b>y)/d.exec("y"): a did not participate *)
  let re = regexp_compile "(?<a>x)|(?<b>y)" ~flags:"d" in
  let m = exec_exn re "y" in
  assert_range "non-participating named group" (RegExp.group_indices "a" m) None;
  assert_range "participating named group"
    (RegExp.group_indices "b" m)
    (Some (0, 1))

let indices_are_utf16 () =
  (* "a😀b": the emoji occupies UTF-16 units 1-2, so "b" is at [3, 4) *)
  let re = regexp_compile "(b)" ~flags:"d" in
  let m = exec_exn re "a😀b" in
  let indices = indices_exn m in
  assert_range "full match" indices.ranges.(0) (Some (3, 4));
  assert_range "group 1" indices.ranges.(1) (Some (3, 4));
  (* the range is expressed in the same unit as match_result.index *)
  assert_int m.index 3

let empty_match_indices () =
  (* /(?:)/d.exec("ab").indices[0] is [0, 0] *)
  let re = regexp_compile "(?:)" ~flags:"d" in
  let m = exec_exn re "ab" in
  let indices = indices_exn m in
  assert_range "empty match" indices.ranges.(0) (Some (0, 0))

let global_iteration_advances_indices () =
  (* /o/dg on "foo": successive execs report successive ranges *)
  let re = regexp_compile "o" ~flags:"dg" in
  let m1 = exec_exn re "foo" in
  assert_range "first match" (indices_exn m1).ranges.(0) (Some (1, 2));
  let m2 = exec_exn re "foo" in
  assert_range "second match" (indices_exn m2).ranges.(0) (Some (2, 3))

let unicode_flag_indices () =
  (* astral char with the u flag still reports UTF-16 units *)
  let re = regexp_compile "(\\u{1F600})" ~flags:"du" in
  let m = exec_exn re "a😀b" in
  let indices = indices_exn m in
  assert_range "full match" indices.ranges.(0) (Some (1, 3));
  assert_range "group 1" indices.ranges.(1) (Some (1, 3))

let tests =
  [
    test "indices is None without the 'd' flag" without_d_flag_indices_is_none;
    test "basic indices" basic_indices;
    test "ranges delimit the captured substrings"
      range_matches_captured_substring;
    test "non-participating group is None" non_participating_group_is_none;
    test "named groups indices" named_groups_indices;
    test "group_indices without 'd' flag is None" group_indices_without_d_flag;
    test "group_indices of non-participating group"
      group_indices_non_participating;
    test "indices are UTF-16 code units" indices_are_utf16;
    test "empty match" empty_match_indices;
    test "global iteration advances indices" global_iteration_advances_indices;
    test "unicode flag reports UTF-16 units" unicode_flag_indices;
  ]
