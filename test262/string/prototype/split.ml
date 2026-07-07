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

let limit_larger_than_parts () =
  let result = String.Prototype.split_limit "," 100 "a,b" in
  assert_array result [| "a"; "b" |]

let negative_limit_is_to_uint32 () =
  (* ToUint32(-1) is 4294967295: effectively no limit *)
  assert_array
    (String.Prototype.split_limit "," (-1) "a,b,c")
    [| "a"; "b"; "c" |];
  (* ToUint32(-(2^32) + 1) is 1 *)
  assert_array
    (String.Prototype.split_limit "," (-4294967295) "a,b,c")
    [| "a" |]

let limit_two_pow_32_wraps_to_zero () =
  (* ToUint32(2^32) is 0 *)
  assert_array (String.Prototype.split_limit "," 4294967296 "a,b,c") [||]

let regex_with_limit () =
  let result = String.Prototype.split_regex_limit "," 2 "a,b,c,d" in
  assert_option_array result [| Some "a"; Some "b" |]

let regex_limit_zero () =
  assert_option_array (String.Prototype.split_regex_limit "," 0 "a,b,c") [||];
  (* the limit check precedes the empty-string check *)
  assert_option_array (String.Prototype.split_regex_limit "x" 0 "") [||]

let regex_limit_counts_captures () =
  (* Spliced capture groups count toward the limit:
     "a,b,c".split(/(,)/, 2) is ["a", ","] *)
  let result = String.Prototype.split_regex_limit "(,)" 2 "a,b,c" in
  assert_option_array result [| Some "a"; Some "," |];
  (* "a,b,c".split(/(,)/, 3) is ["a", ",", "b"] *)
  let result = String.Prototype.split_regex_limit "(,)" 3 "a,b,c" in
  assert_option_array result [| Some "a"; Some ","; Some "b" |]

let regex_limit_negative_is_to_uint32 () =
  let result = String.Prototype.split_regex_limit "," (-1) "a,b,c" in
  assert_option_array result [| Some "a"; Some "b"; Some "c" |]

let regex_limit_empty_pattern () =
  (* "abc".split(/(?:)/, 2) is ["a", "b"] *)
  let result = String.Prototype.split_regex_limit "(?:)" 2 "abc" in
  assert_option_array result [| Some "a"; Some "b" |]

let regex_separator () =
  let result = String.Prototype.split_regex "\\s+" "a b  c\td" in
  assert_option_array result [| Some "a"; Some "b"; Some "c"; Some "d" |]

let regex_with_capture_groups () =
  (* Captured groups are included in result *)
  let result = String.Prototype.split_regex "(,)" "a,b,c" in
  assert_option_array result
    [| Some "a"; Some ","; Some "b"; Some ","; Some "c" |]

let regex_non_participating_capture () =
  (* "ab".split(/(x)?(b)/) is ["a", undefined, "b", ""] *)
  let result = String.Prototype.split_regex "(x)?(b)" "ab" in
  assert_option_array result [| Some "a"; None; Some "b"; Some "" |]

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
    test "non-participating capture is undefined"
      regex_non_participating_capture;
    test "S15.5.4.14_A4_T10: limit larger than parts" limit_larger_than_parts;
    test "S15.5.4.14_A4_T22: negative limit is ToUint32"
      negative_limit_is_to_uint32;
    test "limit 2^32 wraps to zero" limit_two_pow_32_wraps_to_zero;
    test "regex with limit" regex_with_limit;
    test "regex limit zero" regex_limit_zero;
    test "regex limit counts captures" regex_limit_counts_captures;
    test "regex negative limit is ToUint32" regex_limit_negative_is_to_uint32;
    test "regex limit with empty pattern" regex_limit_empty_pattern;
  ]
