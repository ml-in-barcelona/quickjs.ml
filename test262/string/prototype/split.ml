(* TC39 Test262: String.prototype.split tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/split

   ECMA-262 Section: String.prototype.split(separator, limit)

   Tests for String.Prototype.split *)

module String = Quickjs.String

(* ===================================================================
   Basic split functionality - S15.5.4.14
   =================================================================== *)

let string_separator () =
  let result = String.Prototype.split "," "a,b,c" in
  assert_int (Array.length result) 3;
  assert_string result.(0) "a";
  assert_string result.(1) "b";
  assert_string result.(2) "c"

let no_separator_match () =
  let result = String.Prototype.split "," "abc" in
  assert_int (Array.length result) 1;
  assert_string result.(0) "abc"

let empty_separator () =
  (* Split into individual characters (UTF-16 code units) *)
  let result = String.Prototype.split "" "abc" in
  assert_int (Array.length result) 3;
  assert_string result.(0) "a";
  assert_string result.(1) "b";
  assert_string result.(2) "c"

let empty_string () =
  let result = String.Prototype.split "," "" in
  assert_int (Array.length result) 1;
  assert_string result.(0) ""

let with_limit () =
  let result = String.Prototype.split_limit "," 2 "a,b,c,d" in
  assert_int (Array.length result) 2;
  assert_string result.(0) "a";
  assert_string result.(1) "b"

let limit_zero () =
  let result = String.Prototype.split_limit "," 0 "a,b,c" in
  assert_int (Array.length result) 0

let regex_separator () =
  let result = String.Prototype.split_regex "\\s+" "a b  c\td" in
  assert_int (Array.length result) 4;
  assert_string result.(0) "a";
  assert_string result.(1) "b";
  assert_string result.(2) "c";
  assert_string result.(3) "d"

let regex_with_capture_groups () =
  (* Captured groups are included in result *)
  let result = String.Prototype.split_regex "(,)" "a,b,c" in
  assert_int (Array.length result) 5;
  assert_string result.(0) "a";
  assert_string result.(1) ",";
  assert_string result.(2) "b";
  assert_string result.(3) ",";
  assert_string result.(4) "c"

let consecutive_separators () =
  let result = String.Prototype.split "," "a,,b" in
  assert_int (Array.length result) 3;
  assert_string result.(0) "a";
  assert_string result.(1) "";
  assert_string result.(2) "b"

let separator_at_start_end () =
  let result = String.Prototype.split "," ",a,b," in
  assert_int (Array.length result) 4;
  assert_string result.(0) "";
  assert_string result.(1) "a";
  assert_string result.(2) "b";
  assert_string result.(3) ""

let unicode_separator () =
  let result = String.Prototype.split "日" "a日b日c" in
  assert_int (Array.length result) 3;
  assert_string result.(0) "a";
  assert_string result.(1) "b";
  assert_string result.(2) "c"

let unicode_string_empty_separator () =
  (* Split Unicode string into code units *)
  let result = String.Prototype.split "" "日本" in
  assert_int (Array.length result) 2;
  assert_string result.(0) "日";
  assert_string result.(1) "本"

let tests =
  [
    test "S15.5.4.14_A1: string separator" string_separator;
    test "S15.5.4.14_A2: no separator match" no_separator_match;
    test "S15.5.4.14_A3: empty separator" empty_separator;
    test "S15.5.4.14_A4: empty string" empty_string;
    test "S15.5.4.14_A5: with limit" with_limit;
    test "S15.5.4.14_A6: limit zero" limit_zero;
    test "S15.5.4.14_A7: regex separator" regex_separator;
    test "S15.5.4.14_A8: regex with capture groups" regex_with_capture_groups;
    test "S15.5.4.14_A9: consecutive separators" consecutive_separators;
    test "S15.5.4.14_A10: separator at start/end" separator_at_start_end;
    test "S15.5.4.14_A11: Unicode separator" unicode_separator;
    test "S15.5.4.14_A12: Unicode string empty separator"
      unicode_string_empty_separator;
  ]
