(* TC39 Test262: RegExp.prototype.test tests

    Based on: https://github.com/tc39/test262/tree/main/test/built-ins/RegExp/prototype/test

    ECMA-262 Section: RegExp.prototype.test(string)

    Test naming convention follows tc39/test262:
    - S15.10.6.3_A{section}_T{test} format for legacy tests *)

module RegExp = Quickjs.RegExp

(* S15.10.6.3_A1: Basic test functionality *)
(* Returns true if the pattern matches, false otherwise *)

let a1_t1 () =
  (* Simple pattern match *)
  let re = regexp_compile "abc" ~flags:"" in
  assert_bool (RegExp.test re "abc") true;
  assert_bool (RegExp.test re "abcd") true;
  (* contains abc *)
  assert_bool (RegExp.test re "xabcy") true (* contains abc *)

let a1_t2 () =
  (* No match returns false *)
  let re = regexp_compile "abc" ~flags:"" in
  assert_bool (RegExp.test re "def") false;
  assert_bool (RegExp.test re "ab") false;
  (* incomplete *)
  assert_bool (RegExp.test re "") false

let a1_t3 () =
  (* Empty pattern matches everything *)
  let re = regexp_compile "" ~flags:"" in
  assert_bool (RegExp.test re "anything") true;
  assert_bool (RegExp.test re "") true

let a1_t4 () =
  (* Character class *)
  let re = regexp_compile "[a-z]" ~flags:"" in
  assert_bool (RegExp.test re "a") true;
  assert_bool (RegExp.test re "m") true;
  assert_bool (RegExp.test re "z") true;
  assert_bool (RegExp.test re "A") false;
  assert_bool (RegExp.test re "1") false

let a1_t5 () =
  (* Digit pattern *)
  let re = regexp_compile "\\d+" ~flags:"" in
  assert_bool (RegExp.test re "123") true;
  assert_bool (RegExp.test re "abc123xyz") true;
  assert_bool (RegExp.test re "abc") false

let a1_t6 () =
  (* Word boundary *)
  let re = regexp_compile "\\bword\\b" ~flags:"" in
  assert_bool (RegExp.test re "word") true;
  assert_bool (RegExp.test re "a word here") true;
  assert_bool (RegExp.test re "wording") false;
  assert_bool (RegExp.test re "sword") false

let a1_t7 () =
  (* Start anchor *)
  let re = regexp_compile "^abc" ~flags:"" in
  assert_bool (RegExp.test re "abc") true;
  assert_bool (RegExp.test re "abcdef") true;
  assert_bool (RegExp.test re "xabc") false

let a1_t8 () =
  (* End anchor *)
  let re = regexp_compile "abc$" ~flags:"" in
  assert_bool (RegExp.test re "abc") true;
  assert_bool (RegExp.test re "xyzabc") true;
  assert_bool (RegExp.test re "abcx") false

let a1_t9 () =
  (* Both anchors *)
  let re = regexp_compile "^abc$" ~flags:"" in
  assert_bool (RegExp.test re "abc") true;
  assert_bool (RegExp.test re "abcd") false;
  assert_bool (RegExp.test re "xabc") false

let a1_t10 () =
  (* Alternation *)
  let re = regexp_compile "cat|dog" ~flags:"" in
  assert_bool (RegExp.test re "cat") true;
  assert_bool (RegExp.test re "dog") true;
  assert_bool (RegExp.test re "bird") false

let a1_t11 () =
  (* Quantifiers *)
  let re = regexp_compile "a+" ~flags:"" in
  assert_bool (RegExp.test re "a") true;
  assert_bool (RegExp.test re "aaa") true;
  assert_bool (RegExp.test re "bbb") false;

  let re2 = regexp_compile "a*" ~flags:"" in
  assert_bool (RegExp.test re2 "") true;
  (* zero or more *)
  assert_bool (RegExp.test re2 "bbb") true;

  (* matches empty string within *)
  let re3 = regexp_compile "a?" ~flags:"" in
  assert_bool (RegExp.test re3 "a") true;
  assert_bool (RegExp.test re3 "b") true (* matches empty string *)

let a1_t12 () =
  (* Groups *)
  let re = regexp_compile "(ab)+" ~flags:"" in
  assert_bool (RegExp.test re "ab") true;
  assert_bool (RegExp.test re "abab") true;
  (* "aabb" contains "ab" at position 1, so /(ab)+/.test("aabb") returns true *)
  assert_bool (RegExp.test re "aabb") true

let a1_t13 () =
  (* Optional group *)
  let re = regexp_compile "colou?r" ~flags:"" in
  assert_bool (RegExp.test re "color") true;
  assert_bool (RegExp.test re "colour") true;
  assert_bool (RegExp.test re "colouur") false

let a1_t14 () =
  (* Escape sequences *)
  let re = regexp_compile "\\." ~flags:"" in
  assert_bool (RegExp.test re ".") true;
  assert_bool (RegExp.test re "a") false;

  let re2 = regexp_compile "\\$" ~flags:"" in
  assert_bool (RegExp.test re2 "$100") true

let a1_t15 () =
  (* Whitespace *)
  let re = regexp_compile "\\s+" ~flags:"" in
  assert_bool (RegExp.test re " ") true;
  assert_bool (RegExp.test re "\t") true;
  assert_bool (RegExp.test re "\n") true;
  assert_bool (RegExp.test re "abc") false

(* Flag behavior with test() *)

let flag_i () =
  (* Case insensitive flag *)
  let re = regexp_compile "abc" ~flags:"i" in
  assert_bool (RegExp.test re "ABC") true;
  assert_bool (RegExp.test re "AbC") true;
  assert_bool (RegExp.test re "abc") true

let flag_g () =
  (* Global flag - affects lastIndex *)
  let re = regexp_compile "a" ~flags:"g" in
  let input = "aaa" in
  assert_bool (RegExp.test re input) true;
  (* After first test with global flag, lastIndex advances *)
  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) true;
  (* After exhausting matches, returns false and resets *)
  assert_bool (RegExp.test re input) false;
  assert_bool (RegExp.test re input) true (* starts over *)

let flag_m () =
  (* Multiline flag *)
  let re = regexp_compile "^abc" ~flags:"m" in
  assert_bool (RegExp.test re "abc") true;
  assert_bool (RegExp.test re "xyz\nabc") true;

  let re_no_m = regexp_compile "^abc" ~flags:"" in
  assert_bool (RegExp.test re_no_m "xyz\nabc") false

let flag_s () =
  (* Dotall flag - dot matches newlines *)
  let re = regexp_compile "a.b" ~flags:"s" in
  assert_bool (RegExp.test re "a\nb") true;

  let re_no_s = regexp_compile "a.b" ~flags:"" in
  assert_bool (RegExp.test re_no_s "a\nb") false

let flag_y () =
  (* Sticky flag - must reset lastIndex between tests because sticky flag
     advances lastIndex after each successful match *)
  let re = regexp_compile "a" ~flags:"y" in
  assert_bool (RegExp.test re "abc") true;
  (* matches at position 0, lastIndex becomes 1 *)
  (* Reset lastIndex to test from position 0 again *)
  RegExp.setLastIndex re 0;
  assert_bool (RegExp.test re "bac") false (* doesn't match at position 0 *)

let flag_y_lastindex () =
  (* Sticky flag with lastIndex *)
  let re = regexp_compile "a" ~flags:"y" in
  RegExp.setLastIndex re 1;
  assert_bool (RegExp.test re "bac") true;
  (* matches at position 1 *)
  assert_int (RegExp.lastIndex re) 2

(* Edge cases *)

let special_chars () =
  (* Special regex characters in pattern *)
  let re = regexp_compile "\\[\\]" ~flags:"" in
  assert_bool (RegExp.test re "[]") true;

  let re2 = regexp_compile "\\(\\)" ~flags:"" in
  assert_bool (RegExp.test re2 "()") true;

  let re3 = regexp_compile "\\{\\}" ~flags:"" in
  assert_bool (RegExp.test re3 "{}") true

let unicode_basic () =
  (* Basic unicode matching *)
  let re = regexp_compile "café" ~flags:"" in
  assert_bool (RegExp.test re "café") true;
  assert_bool (RegExp.test re "cafe") false

let empty_string_match () =
  (* Matching empty string *)
  let re = regexp_compile "^$" ~flags:"" in
  assert_bool (RegExp.test re "") true;
  assert_bool (RegExp.test re "a") false

let lookahead () =
  (* Positive lookahead *)
  let re = regexp_compile "a(?=b)" ~flags:"" in
  assert_bool (RegExp.test re "ab") true;
  assert_bool (RegExp.test re "ac") false;

  (* Negative lookahead *)
  let re2 = regexp_compile "a(?!b)" ~flags:"" in
  assert_bool (RegExp.test re2 "ac") true;
  assert_bool (RegExp.test re2 "ab") false

let lookbehind () =
  (* Positive lookbehind *)
  let re = regexp_compile "(?<=a)b" ~flags:"" in
  assert_bool (RegExp.test re "ab") true;
  assert_bool (RegExp.test re "cb") false;

  (* Negative lookbehind *)
  let re2 = regexp_compile "(?<!a)b" ~flags:"" in
  assert_bool (RegExp.test re2 "cb") true;
  assert_bool (RegExp.test re2 "ab") false

let backreference () =
  (* Backreference in pattern *)
  let re = regexp_compile "(a)\\1" ~flags:"" in
  assert_bool (RegExp.test re "aa") true;
  assert_bool (RegExp.test re "ab") false

let repetition_bounds () =
  (* Bounded repetition *)
  let re = regexp_compile "a{2,4}" ~flags:"" in
  assert_bool (RegExp.test re "a") false;
  assert_bool (RegExp.test re "aa") true;
  assert_bool (RegExp.test re "aaa") true;
  assert_bool (RegExp.test re "aaaa") true;
  assert_bool (RegExp.test re "aaaaa") true (* greedy, matches first 4 *)

(* ===================================================================
   Sticky vs Global flag comparison
   =================================================================== *)

let sticky_vs_global () =
  let input = "abc xyz abc" in
  let sticky = regexp_compile "abc" ~flags:"y" in
  let global = regexp_compile "abc" ~flags:"g" in
  (* Global finds matches anywhere in the string *)
  assert_bool (RegExp.test global input) true;
  assert_bool (RegExp.test global input) true;
  (* Sticky only matches at lastIndex position *)
  assert_bool (RegExp.test sticky input) true;
  (* After first match at 0, lastIndex is 3, "xyz" is at 3, so no match *)
  assert_bool (RegExp.test sticky input) false

(* ===================================================================
   Unicode property escapes
   =================================================================== *)

let unicode_ascii_class_no_unicode () =
  (* [a-z] does NOT match unicode letters without u flag *)
  let re = regexp_compile "^[a-z]+$" ~flags:"i" in
  (* ASCII works *)
  assert_bool (RegExp.test re "car") true;
  (* Unicode letters don't match [a-z] *)
  assert_bool (RegExp.test re "pão") false;
  assert_bool (RegExp.test re "知道") false;
  assert_bool (RegExp.test re "يعرف") false

let unicode_property_escape () =
  (* \p{L} matches unicode letters with u flag *)
  let re = regexp_compile "^\\p{L}+$" ~flags:"u" in
  (* ASCII works *)
  assert_bool (RegExp.test re "car") true;
  (* Unicode letters match with \p{L} and u flag *)
  assert_bool (RegExp.test re "pão") true;
  assert_bool (RegExp.test re "知道") true;
  assert_bool (RegExp.test re "يعرف") true

(* ===================================================================
   Complex pattern tests
   =================================================================== *)

let http_url_pattern () =
  let pattern = "^[https?]+:\\/\\/((w{3}\\.)?[\\w+]+)\\.[\\w+]+$" in
  let re = regexp_compile pattern ~flags:"" in
  assert_bool (RegExp.test re "https://www.example.com") true;
  assert_bool (RegExp.test re "http://example.com") true;
  assert_bool (RegExp.test re "https://example") false

let tests =
  [
    (* A1: Basic functionality *)
    test "S15.10.6.3_A1_T1: simple match" a1_t1;
    test "S15.10.6.3_A1_T2: no match" a1_t2;
    test "S15.10.6.3_A1_T3: empty pattern" a1_t3;
    test "S15.10.6.3_A1_T4: character class" a1_t4;
    test "S15.10.6.3_A1_T5: digit pattern" a1_t5;
    test "S15.10.6.3_A1_T6: word boundary" a1_t6;
    test "S15.10.6.3_A1_T7: start anchor" a1_t7;
    test "S15.10.6.3_A1_T8: end anchor" a1_t8;
    test "S15.10.6.3_A1_T9: both anchors" a1_t9;
    test "S15.10.6.3_A1_T10: alternation" a1_t10;
    test "S15.10.6.3_A1_T11: quantifiers" a1_t11;
    test "S15.10.6.3_A1_T12: groups" a1_t12;
    test "S15.10.6.3_A1_T13: optional group" a1_t13;
    test "S15.10.6.3_A1_T14: escape sequences" a1_t14;
    test "S15.10.6.3_A1_T15: whitespace" a1_t15;
    (* Flag behavior *)
    test "flag_i: case insensitive" flag_i;
    test "flag_g: global" flag_g;
    test "flag_m: multiline" flag_m;
    test "flag_s: dotall" flag_s;
    test "flag_y: sticky" flag_y;
    test "flag_y_lastindex: sticky with lastIndex" flag_y_lastindex;
    test "sticky_vs_global: sticky vs global behavior" sticky_vs_global;
    (* Edge cases *)
    test "special_chars: regex metacharacters" special_chars;
    test "unicode_basic: unicode matching" unicode_basic;
    test "empty_string_match: empty string" empty_string_match;
    test "lookahead: positive and negative" lookahead;
    test "lookbehind: positive and negative" lookbehind;
    test "backreference: backreference" backreference;
    test "repetition_bounds: bounded repetition" repetition_bounds;
    (* Unicode *)
    test "unicode_ascii_class: [a-z] vs unicode" unicode_ascii_class_no_unicode;
    test "unicode_property_escape: \\p{L} matches letters"
      unicode_property_escape;
    (* Complex patterns *)
    test "http_url_pattern: URL matching" http_url_pattern;
  ]
