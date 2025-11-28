(** TC39 Test262: RegExp.prototype.exec tests

    Based on: https://github.com/tc39/test262/tree/main/test/built-ins/RegExp/prototype/exec

    ECMA-262 Section: RegExp.prototype.exec(string)

    Test naming convention follows tc39/test262:
    - S15.10.6.2_A{section}_T{test} format for legacy tests *)

module RegExp = Quickjs.RegExp

(* ===================================================================
   S15.10.6.2_A1: Basic exec functionality
   Returns array with match info, or empty array if no match
   =================================================================== *)

let a1_t1 () =
  (* Simple match *)
  let re = regexp_compile "abc" ~flags:"" in
  let result = RegExp.exec re "abc" in
  assert_array (RegExp.captures result) [| "abc" |];
  assert_int (RegExp.index result) 0

let a1_t2 () =
  (* Match within string *)
  let re = regexp_compile "bc" ~flags:"" in
  let result = RegExp.exec re "abcd" in
  assert_array (RegExp.captures result) [| "bc" |];
  assert_int (RegExp.index result) 1

let a1_t3 () =
  (* No match returns empty captures *)
  let re = regexp_compile "xyz" ~flags:"" in
  let result = RegExp.exec re "abc" in
  assert_array (RegExp.captures result) [||]

let a1_t4 () =
  (* Multiple potential matches - returns first *)
  let re = regexp_compile "a" ~flags:"" in
  let result = RegExp.exec re "banana" in
  assert_array (RegExp.captures result) [| "a" |];
  assert_int (RegExp.index result) 1

let a1_t5 () =
  (* Digit pattern *)
  let re = regexp_compile "\\d+" ~flags:"" in
  let result = RegExp.exec re "abc123def" in
  assert_array (RegExp.captures result) [| "123" |];
  assert_int (RegExp.index result) 3

let a1_t6 () =
  (* Word pattern *)
  let re = regexp_compile "\\w+" ~flags:"" in
  let result = RegExp.exec re "  hello world  " in
  assert_array (RegExp.captures result) [| "hello" |];
  assert_int (RegExp.index result) 2

let a1_t7 () =
  (* Empty input with non-empty pattern *)
  let re = regexp_compile "a" ~flags:"" in
  let result = RegExp.exec re "" in
  assert_array (RegExp.captures result) [||]

let a1_t8 () =
  (* Empty pattern matches at start *)
  let re = regexp_compile "" ~flags:"" in
  let result = RegExp.exec re "abc" in
  assert_array (RegExp.captures result) [| "" |];
  assert_int (RegExp.index result) 0

let a1_t9 () =
  (* Case sensitivity *)
  let re = regexp_compile "ABC" ~flags:"" in
  let result = RegExp.exec re "abc ABC def" in
  assert_array (RegExp.captures result) [| "ABC" |];
  assert_int (RegExp.index result) 4

let a1_t10 () =
  (* Special characters in input *)
  let re = regexp_compile "\\$" ~flags:"" in
  let result = RegExp.exec re "price: $100" in
  assert_array (RegExp.captures result) [| "$" |];
  assert_int (RegExp.index result) 7

(* ===================================================================
   S15.10.6.2_A2: Capture groups
   =================================================================== *)

let a2_t1 () =
  (* Single capture group *)
  let re = regexp_compile "(abc)" ~flags:"" in
  let result = RegExp.exec re "abc" in
  assert_array (RegExp.captures result) [| "abc"; "abc" |]

let a2_t2 () =
  (* Multiple capture groups *)
  let re = regexp_compile "(a)(b)(c)" ~flags:"" in
  let result = RegExp.exec re "abc" in
  assert_array (RegExp.captures result) [| "abc"; "a"; "b"; "c" |]

let a2_t3 () =
  (* Nested capture groups *)
  let re = regexp_compile "((a)(b))" ~flags:"" in
  let result = RegExp.exec re "ab" in
  assert_array (RegExp.captures result) [| "ab"; "ab"; "a"; "b" |]

let a2_t4 () =
  (* Optional group that doesn't match *)
  let re = regexp_compile "a(b)?c" ~flags:"" in
  let result = RegExp.exec re "ac" in
  assert_array (RegExp.captures result) [| "ac"; "" |]
(* group 1 is empty *)

let a2_t5 () =
  (* Multiple groups with some not matching *)
  let re = regexp_compile "(a)|(b)" ~flags:"" in
  let result = RegExp.exec re "a" in
  (* First alternative matches, second doesn't *)
  assert_array (RegExp.captures result) [| "a"; "a"; "" |]

let a2_t6 () =
  (* Quantified group *)
  let re = regexp_compile "(ab)+" ~flags:"" in
  let result = RegExp.exec re "abab" in
  assert_array (RegExp.captures result) [| "abab"; "ab" |]
(* last match of group *)

let a2_t7 () =
  (* Group with alternation *)
  let re = regexp_compile "(cat|dog)" ~flags:"" in
  let result = RegExp.exec re "I have a dog" in
  assert_array (RegExp.captures result) [| "dog"; "dog" |];
  assert_int (RegExp.index result) 9

let a2_t8 () =
  (* Non-capturing group *)
  let re = regexp_compile "(?:a)(b)" ~flags:"" in
  let result = RegExp.exec re "ab" in
  assert_array (RegExp.captures result) [| "ab"; "b" |]
(* only one capture *)

let a2_t9 () =
  (* Complex pattern with groups *)
  let re = regexp_compile "(\\d{4})-(\\d{2})-(\\d{2})" ~flags:"" in
  let result = RegExp.exec re "Date: 2024-07-17" in
  assert_array (RegExp.captures result) [| "2024-07-17"; "2024"; "07"; "17" |];
  assert_int (RegExp.index result) 6

let a2_t10 () =
  (* Email-like pattern *)
  let re = regexp_compile "(\\w+)@(\\w+)\\.(\\w+)" ~flags:"" in
  let result = RegExp.exec re "email: test@example.com" in
  assert_array (RegExp.captures result)
    [| "test@example.com"; "test"; "example"; "com" |]

(* ===================================================================
   S15.10.6.2_A3: Global flag and lastIndex
   =================================================================== *)

let a3_t1 () =
  (* Global flag - successive calls advance through string *)
  let re = regexp_compile "a" ~flags:"g" in
  let input = "banana" in

  let result1 = RegExp.exec re input in
  assert_array (RegExp.captures result1) [| "a" |];
  assert_int (RegExp.index result1) 1;
  assert_int (RegExp.lastIndex re) 2;

  let result2 = RegExp.exec re input in
  assert_array (RegExp.captures result2) [| "a" |];
  assert_int (RegExp.index result2) 3;
  assert_int (RegExp.lastIndex re) 4;

  let result3 = RegExp.exec re input in
  assert_array (RegExp.captures result3) [| "a" |];
  assert_int (RegExp.index result3) 5;
  assert_int (RegExp.lastIndex re) 6;

  (* No more matches - returns empty and resets lastIndex *)
  let result4 = RegExp.exec re input in
  assert_array (RegExp.captures result4) [||];
  assert_int (RegExp.lastIndex re) 0

let a3_t2 () =
  (* Without global flag - always starts from beginning *)
  let re = regexp_compile "a" ~flags:"" in
  let input = "banana" in

  let result1 = RegExp.exec re input in
  assert_int (RegExp.index result1) 1;

  let result2 = RegExp.exec re input in
  assert_int (RegExp.index result2) 1 (* same position *)

let a3_t3 () =
  (* Manual lastIndex manipulation with global *)
  let re = regexp_compile "\\d+" ~flags:"g" in
  let input = "a1b22c333d" in

  RegExp.setLastIndex re 3;
  (* Start after "a1b" *)
  let result = RegExp.exec re input in
  assert_array (RegExp.captures result) [| "22" |];
  assert_int (RegExp.index result) 3

let a3_t4 () =
  (* lastIndex beyond string length *)
  let re = regexp_compile "a" ~flags:"g" in
  let input = "abc" in

  RegExp.setLastIndex re 100;
  let result = RegExp.exec re input in
  assert_array (RegExp.captures result) [||];
  assert_int (RegExp.lastIndex re) 0

let a3_t5 () =
  (* lastIndex with global flag iterating all matches *)
  let re = regexp_compile "\\d+" ~flags:"g" in
  let input = "1 22 333 4444" in

  let result1 = RegExp.exec re input in
  assert_array (RegExp.captures result1) [| "1" |];

  let result2 = RegExp.exec re input in
  assert_array (RegExp.captures result2) [| "22" |];

  let result3 = RegExp.exec re input in
  assert_array (RegExp.captures result3) [| "333" |];

  let result4 = RegExp.exec re input in
  assert_array (RegExp.captures result4) [| "4444" |];

  let result5 = RegExp.exec re input in
  assert_array (RegExp.captures result5) [||]

let a3_t6 () =
  (* Sticky flag starts from lastIndex exactly *)
  let re = regexp_compile "abc" ~flags:"y" in
  let input = "abcabc" in

  let result1 = RegExp.exec re input in
  assert_array (RegExp.captures result1) [| "abc" |];
  assert_int (RegExp.index result1) 0;

  let result2 = RegExp.exec re input in
  assert_array (RegExp.captures result2) [| "abc" |];
  assert_int (RegExp.index result2) 3;

  let result3 = RegExp.exec re input in
  assert_array (RegExp.captures result3) [||]

let a3_t7 () =
  (* Sticky flag fails if not at lastIndex position *)
  let re = regexp_compile "b" ~flags:"y" in
  let input = "abc" in

  let result = RegExp.exec re input in
  assert_array (RegExp.captures result) [||];

  (* b is not at position 0 *)
  RegExp.setLastIndex re 1;
  let result2 = RegExp.exec re input in
  assert_array (RegExp.captures result2) [| "b" |]
(* now it matches *)

(* ===================================================================
   S15.10.6.2_A4: Named capture groups
   =================================================================== *)

let a4_t1 () =
  (* Basic named group *)
  let re = regexp_compile "(?<name>\\w+)" ~flags:"" in
  let result = RegExp.exec re "hello" in
  assert_array (RegExp.captures result) [| "hello"; "hello" |];
  assert_string (Option.get (RegExp.group "name" result)) "hello"

let a4_t2 () =
  (* Multiple named groups *)
  let re = regexp_compile "(?<first>\\w+) (?<last>\\w+)" ~flags:"" in
  let result = RegExp.exec re "John Doe" in
  assert_array (RegExp.captures result) [| "John Doe"; "John"; "Doe" |];
  assert_string (Option.get (RegExp.group "first" result)) "John";
  assert_string (Option.get (RegExp.group "last" result)) "Doe"

let a4_t3 () =
  (* Named groups with date pattern *)
  let re =
    regexp_compile "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})" ~flags:""
  in
  let result = RegExp.exec re "2024-07-17" in
  assert_string (Option.get (RegExp.group "year" result)) "2024";
  assert_string (Option.get (RegExp.group "month" result)) "07";
  assert_string (Option.get (RegExp.group "day" result)) "17"

let a4_t4 () =
  (* Non-existent named group returns None *)
  let re = regexp_compile "(?<name>\\w+)" ~flags:"" in
  let result = RegExp.exec re "hello" in
  assert_bool (Option.is_none (RegExp.group "nonexistent" result)) true

let a4_t5 () =
  (* Named group that doesn't participate in match *)
  let re = regexp_compile "(?<a>a)|(?<b>b)" ~flags:"" in
  let result = RegExp.exec re "b" in
  assert_string (Option.get (RegExp.group "b" result)) "b";
  (* Group "a" didn't match - should be None or empty *)
  let groups = RegExp.groups result in
  assert_int (List.length groups) 2

let a4_t6 () =
  (* Mixed named and unnamed groups *)
  let re = regexp_compile "(\\d+)-(?<name>\\w+)-(\\d+)" ~flags:"" in
  let result = RegExp.exec re "123-test-456" in
  assert_array (RegExp.captures result)
    [| "123-test-456"; "123"; "test"; "456" |];
  assert_string (Option.get (RegExp.group "name" result)) "test"

(* ===================================================================
   S15.10.6.2_A5: Various patterns
   =================================================================== *)

let a5_t1 () =
  (* Anchors *)
  let re = regexp_compile "^hello" ~flags:"" in
  let result = RegExp.exec re "hello world" in
  assert_array (RegExp.captures result) [| "hello" |];
  assert_int (RegExp.index result) 0

let a5_t2 () =
  (* End anchor *)
  let re = regexp_compile "world$" ~flags:"" in
  let result = RegExp.exec re "hello world" in
  assert_array (RegExp.captures result) [| "world" |];
  assert_int (RegExp.index result) 6

let a5_t3 () =
  (* Multiline anchor *)
  let re = regexp_compile "^line" ~flags:"m" in
  let input = "first\nline two\nline three" in
  let result = RegExp.exec re input in
  assert_array (RegExp.captures result) [| "line" |];
  assert_int (RegExp.index result) 6

(* ===================================================================
   Edge cases
   =================================================================== *)

let edge_empty_match () =
  (* Pattern that matches empty string *)
  let re = regexp_compile "a*" ~flags:"" in
  let result = RegExp.exec re "bbb" in
  (* Matches empty string at start *)
  assert_array (RegExp.captures result) [| "" |];
  assert_int (RegExp.index result) 0

let edge_unicode () =
  (* Unicode characters *)
  let re = regexp_compile "café" ~flags:"" in
  let result = RegExp.exec re "I love café" in
  assert_array (RegExp.captures result) [| "café" |]

let edge_newlines () =
  (* Newlines in input *)
  let re = regexp_compile "line" ~flags:"" in
  let result = RegExp.exec re "first\nline\nthird" in
  assert_array (RegExp.captures result) [| "line" |];
  assert_int (RegExp.index result) 6

let edge_lookahead () =
  (* Lookahead in exec *)
  let re = regexp_compile "a(?=b)" ~flags:"" in
  let result = RegExp.exec re "ab ac" in
  assert_array (RegExp.captures result) [| "a" |];
  assert_int (RegExp.index result) 0

let edge_lookbehind () =
  (* Lookbehind in exec *)
  let re = regexp_compile "(?<=a)b" ~flags:"" in
  let result = RegExp.exec re "ab cb" in
  assert_array (RegExp.captures result) [| "b" |];
  assert_int (RegExp.index result) 1

let edge_backreference () =
  (* Backreference *)
  let re = regexp_compile "(\\w)\\1" ~flags:"" in
  let result = RegExp.exec re "hello" in
  assert_array (RegExp.captures result) [| "ll"; "l" |];
  assert_int (RegExp.index result) 2

let edge_quantifier_greedy () =
  (* Greedy vs non-greedy *)
  let re_greedy = regexp_compile "a+" ~flags:"" in
  let result_greedy = RegExp.exec re_greedy "aaa" in
  assert_array (RegExp.captures result_greedy) [| "aaa" |];

  let re_lazy = regexp_compile "a+?" ~flags:"" in
  let result_lazy = RegExp.exec re_lazy "aaa" in
  assert_array (RegExp.captures result_lazy) [| "a" |]

let edge_alternation () =
  (* Alternation order *)
  let re = regexp_compile "abc|ab|a" ~flags:"" in
  let result = RegExp.exec re "abc" in
  assert_array (RegExp.captures result) [| "abc" |]
(* first alternative wins *)

let edge_word_boundary () =
  (* Word boundary *)
  let re = regexp_compile "\\bword\\b" ~flags:"" in

  let result1 = RegExp.exec re "the word here" in
  assert_array (RegExp.captures result1) [| "word" |];

  let result2 = RegExp.exec re "wording" in
  assert_array (RegExp.captures result2) [||]

let edge_dotall () =
  (* Dotall flag with exec *)
  let re = regexp_compile "a.b" ~flags:"s" in
  let result = RegExp.exec re "a\nb" in
  assert_array (RegExp.captures result) [| "a\nb" |]

let tests =
  [
    (* A1: Basic exec *)
    test "S15.10.6.2_A1_T1: simple match" a1_t1;
    test "S15.10.6.2_A1_T2: match within string" a1_t2;
    test "S15.10.6.2_A1_T3: no match" a1_t3;
    test "S15.10.6.2_A1_T4: returns first match" a1_t4;
    test "S15.10.6.2_A1_T5: digit pattern" a1_t5;
    test "S15.10.6.2_A1_T6: word pattern" a1_t6;
    test "S15.10.6.2_A1_T7: empty input" a1_t7;
    test "S15.10.6.2_A1_T8: empty pattern" a1_t8;
    test "S15.10.6.2_A1_T9: case sensitivity" a1_t9;
    test "S15.10.6.2_A1_T10: special chars" a1_t10;
    (* A2: Capture groups *)
    test "S15.10.6.2_A2_T1: single group" a2_t1;
    test "S15.10.6.2_A2_T2: multiple groups" a2_t2;
    test "S15.10.6.2_A2_T3: nested groups" a2_t3;
    test "S15.10.6.2_A2_T4: optional group" a2_t4;
    test "S15.10.6.2_A2_T5: groups in alternation" a2_t5;
    test "S15.10.6.2_A2_T6: quantified group" a2_t6;
    test "S15.10.6.2_A2_T7: group with alternation" a2_t7;
    test "S15.10.6.2_A2_T8: non-capturing group" a2_t8;
    test "S15.10.6.2_A2_T9: date pattern" a2_t9;
    test "S15.10.6.2_A2_T10: email pattern" a2_t10;
    (* A3: Global flag and lastIndex *)
    test "S15.10.6.2_A3_T1: global iterates matches" a3_t1;
    test "S15.10.6.2_A3_T2: without global" a3_t2;
    test "S15.10.6.2_A3_T3: manual lastIndex" a3_t3;
    test "S15.10.6.2_A3_T4: lastIndex beyond length" a3_t4;
    test "S15.10.6.2_A3_T5: iterate all matches" a3_t5;
    test "S15.10.6.2_A3_T6: sticky flag" a3_t6;
    test "S15.10.6.2_A3_T7: sticky at position" a3_t7;
    (* A4: Named groups *)
    test "S15.10.6.2_A4_T1: basic named group" a4_t1;
    test "S15.10.6.2_A4_T2: multiple named groups" a4_t2;
    test "S15.10.6.2_A4_T3: named date groups" a4_t3;
    test "S15.10.6.2_A4_T4: nonexistent group" a4_t4;
    test "S15.10.6.2_A4_T5: unmatched named group" a4_t5;
    test "S15.10.6.2_A4_T6: mixed groups" a4_t6;
    (* A5: Various patterns *)
    test "S15.10.6.2_A5_T1: start anchor" a5_t1;
    test "S15.10.6.2_A5_T2: end anchor" a5_t2;
    test "S15.10.6.2_A5_T3: multiline anchor" a5_t3;
    (* Edge cases *)
    test "edge: empty match" edge_empty_match;
    test "edge: unicode" edge_unicode;
    test "edge: newlines" edge_newlines;
    test "edge: lookahead" edge_lookahead;
    test "edge: lookbehind" edge_lookbehind;
    test "edge: backreference" edge_backreference;
    test "edge: greedy vs lazy" edge_quantifier_greedy;
    test "edge: alternation" edge_alternation;
    test "edge: word boundary" edge_word_boundary;
    test "edge: dotall" edge_dotall;
  ]
