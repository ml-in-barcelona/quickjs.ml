(** TC39 Test262: RegExp compile error tests

    Based on: https://github.com/tc39/test262/tree/main/test/built-ins/RegExp

    Tests for RegExp compilation errors and edge cases *)

module RegExp = Quickjs.RegExp

(* ===================================================================
   Successful compilation tests
   =================================================================== *)

let compile_simple () =
  (* Simple patterns should compile - we verify they return a valid RegExp *)
  let re = regexp_compile "abc" ~flags:"" in
  assert_string (RegExp.source re) "abc";
  let re = regexp_compile "\\d+" ~flags:"" in
  assert_string (RegExp.source re) "\\d+";
  let re = regexp_compile "[a-z]" ~flags:"" in
  assert_string (RegExp.source re) "[a-z]"

let compile_empty () =
  (* Empty pattern should compile *)
  let re = regexp_compile "" ~flags:"" in
  assert_string (RegExp.source re) ""

let compile_all_flags () =
  (* All valid flags should work *)
  let re = regexp_compile "abc" ~flags:"g" in
  assert_bool (RegExp.global re) true;
  let re = regexp_compile "abc" ~flags:"i" in
  assert_bool (RegExp.ignorecase re) true;
  let re = regexp_compile "abc" ~flags:"m" in
  assert_bool (RegExp.multiline re) true;
  let re = regexp_compile "abc" ~flags:"s" in
  assert_bool (RegExp.dotall re) true;
  let re = regexp_compile "abc" ~flags:"u" in
  assert_bool (RegExp.unicode re) true;
  let re = regexp_compile "abc" ~flags:"y" in
  assert_bool (RegExp.sticky re) true;
  let re = regexp_compile "abc" ~flags:"gimsuy" in
  assert_bool (RegExp.global re) true;
  assert_bool (RegExp.ignorecase re) true;
  assert_bool (RegExp.multiline re) true;
  assert_bool (RegExp.dotall re) true;
  assert_bool (RegExp.unicode re) true;
  assert_bool (RegExp.sticky re) true

let compile_complex_patterns () =
  (* Complex patterns should compile *)
  let re = regexp_compile "^[a-zA-Z_][a-zA-Z0-9_]*$" ~flags:"" in
  assert_string (RegExp.source re) "^[a-zA-Z_][a-zA-Z0-9_]*$";
  let re = regexp_compile "(\\d{4})-(\\d{2})-(\\d{2})" ~flags:"" in
  assert_string (RegExp.source re) "(\\d{4})-(\\d{2})-(\\d{2})";
  let re = regexp_compile "(?<name>\\w+)" ~flags:"" in
  assert_string (RegExp.source re) "(?<name>\\w+)";
  let re = regexp_compile "a(?=b)c" ~flags:"" in
  assert_string (RegExp.source re) "a(?=b)c";
  let re = regexp_compile "(?<=a)b" ~flags:"" in
  assert_string (RegExp.source re) "(?<=a)b"

(* ===================================================================
   Compilation error tests
   =================================================================== *)

let error_unmatched_paren () =
  (* Unmatched opening parenthesis -> "expecting ')'" *)
  let error = regexp_no_compile "(abc" ~flags:"" in
  assert_unknown_error ~contains:"expecting ')'" error;
  (* Unmatched closing parenthesis -> "extraneous characters" *)
  let error = regexp_no_compile "abc)" ~flags:"" in
  assert_unknown_error ~contains:"extraneous" error

let error_unmatched_bracket () =
  (* Unmatched bracket -> "unexpected end" *)
  let error = regexp_no_compile "[abc" ~flags:"" in
  assert_unexpected_end error

let error_invalid_quantifier () =
  (* Nothing to repeat - quantifier without preceding element *)
  let error = regexp_no_compile "*" ~flags:"" in
  assert_nothing_to_repeat error;
  let error = regexp_no_compile "+" ~flags:"" in
  assert_nothing_to_repeat error;
  let error = regexp_no_compile "?" ~flags:"" in
  assert_nothing_to_repeat error;
  let error = regexp_no_compile "{2,3}" ~flags:"" in
  assert_nothing_to_repeat error

let error_invalid_escape () =
  (* Invalid escape at end of pattern -> "unexpected end" *)
  let error = regexp_no_compile "\\" ~flags:"" in
  assert_unexpected_end error

let error_invalid_group () =
  (* Empty group name -> "invalid group name" *)
  let error = regexp_no_compile "(?<>abc)" ~flags:"" in
  assert_unknown_error ~contains:"invalid group name" error;
  (* Numeric group name -> "invalid group name" *)
  let error = regexp_no_compile "(?<123>abc)" ~flags:"" in
  assert_unknown_error ~contains:"invalid group name" error

let error_empty_alternation () =
  (* Empty alternatives are valid in regex - they match empty string *)
  let re = regexp_compile "a|" ~flags:"" in
  assert_string (RegExp.source re) "a|";
  let re = regexp_compile "|b" ~flags:"" in
  assert_string (RegExp.source re) "|b"

let error_invalid_range () =
  (* Invalid character class range (reversed) -> "invalid class range" *)
  let error = regexp_no_compile "[z-a]" ~flags:"" in
  assert_unknown_error ~contains:"invalid class range" error

let error_invalid_backreference () =
  (* Backreference to non-existent group - QuickJS allows this *)
  let re = regexp_compile "\\1" ~flags:"" in
  assert_string (RegExp.source re) "\\1"

(* ===================================================================
   Edge case patterns - all should compile successfully
   =================================================================== *)

let edge_nested_groups () =
  (* Deeply nested groups *)
  let re = regexp_compile "((((a))))" ~flags:"" in
  assert_string (RegExp.source re) "((((a))))";
  let re = regexp_compile "(a(b(c(d))))" ~flags:"" in
  assert_string (RegExp.source re) "(a(b(c(d))))"

let edge_many_alternatives () =
  (* Many alternatives *)
  let re = regexp_compile "a|b|c|d|e|f|g|h|i|j" ~flags:"" in
  assert_string (RegExp.source re) "a|b|c|d|e|f|g|h|i|j"

let edge_long_pattern () =
  (* Long pattern *)
  let pattern = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" in
  let re = regexp_compile pattern ~flags:"" in
  assert_string (RegExp.source re) pattern

let edge_unicode_pattern () =
  (* Unicode in pattern *)
  let re = regexp_compile "café" ~flags:"" in
  assert_string (RegExp.source re) "café";
  let re = regexp_compile "日本語" ~flags:"" in
  assert_string (RegExp.source re) "日本語";
  let re = regexp_compile "🎉" ~flags:"" in
  assert_string (RegExp.source re) "🎉"

let edge_escape_sequences () =
  (* All escape sequences *)
  let re = regexp_compile "\\d\\D\\w\\W\\s\\S\\b\\B" ~flags:"" in
  assert_string (RegExp.source re) "\\d\\D\\w\\W\\s\\S\\b\\B";
  let re = regexp_compile "\\t\\n\\r\\v\\f" ~flags:"" in
  assert_string (RegExp.source re) "\\t\\n\\r\\v\\f";
  let re = regexp_compile "\\0" ~flags:"" in
  assert_string (RegExp.source re) "\\0"

let edge_character_class_escapes () =
  (* Escapes in character class *)
  let re = regexp_compile "[\\d\\w\\s]" ~flags:"" in
  assert_string (RegExp.source re) "[\\d\\w\\s]";
  let re = regexp_compile "[\\-\\]]" ~flags:"" in
  assert_string (RegExp.source re) "[\\-\\]]"

let edge_quantifier_variations () =
  (* Various quantifier forms *)
  let re = regexp_compile "a{0}" ~flags:"" in
  assert_string (RegExp.source re) "a{0}";
  let re = regexp_compile "a{1}" ~flags:"" in
  assert_string (RegExp.source re) "a{1}";
  let re = regexp_compile "a{2,}" ~flags:"" in
  assert_string (RegExp.source re) "a{2,}";
  let re = regexp_compile "a{2,5}" ~flags:"" in
  assert_string (RegExp.source re) "a{2,5}";
  let re = regexp_compile "a{0,0}" ~flags:"" in
  assert_string (RegExp.source re) "a{0,0}"

let edge_lookaround () =
  (* Lookahead and lookbehind *)
  let re = regexp_compile "(?=abc)" ~flags:"" in
  assert_string (RegExp.source re) "(?=abc)";
  let re = regexp_compile "(?!abc)" ~flags:"" in
  assert_string (RegExp.source re) "(?!abc)";
  let re = regexp_compile "(?<=abc)" ~flags:"" in
  assert_string (RegExp.source re) "(?<=abc)";
  let re = regexp_compile "(?<!abc)" ~flags:"" in
  assert_string (RegExp.source re) "(?<!abc)"

let edge_non_capturing_group () =
  (* Non-capturing groups *)
  let re = regexp_compile "(?:abc)" ~flags:"" in
  assert_string (RegExp.source re) "(?:abc)";
  let re = regexp_compile "(?:a(?:b(?:c)))" ~flags:"" in
  assert_string (RegExp.source re) "(?:a(?:b(?:c)))"

let edge_named_groups () =
  (* Named groups *)
  let re = regexp_compile "(?<name>abc)" ~flags:"" in
  assert_string (RegExp.source re) "(?<name>abc)";
  let re = regexp_compile "(?<first>a)(?<second>b)" ~flags:"" in
  assert_string (RegExp.source re) "(?<first>a)(?<second>b)"

let edge_word_boundaries () =
  (* Word boundaries *)
  let re = regexp_compile "\\bword\\b" ~flags:"" in
  assert_string (RegExp.source re) "\\bword\\b";
  let re = regexp_compile "\\Bword\\B" ~flags:"" in
  assert_string (RegExp.source re) "\\Bword\\B"

let edge_anchors () =
  (* Anchors *)
  let re = regexp_compile "^abc$" ~flags:"" in
  assert_string (RegExp.source re) "^abc$";
  let re = regexp_compile "^$" ~flags:"" in
  assert_string (RegExp.source re) "^$";
  let re = regexp_compile "^^$$" ~flags:"" in
  assert_string (RegExp.source re) "^^$$"

let tests =
  [
    (* Successful compilation *)
    test "compile: simple patterns" compile_simple;
    test "compile: empty pattern" compile_empty;
    test "compile: all flags" compile_all_flags;
    test "compile: complex patterns" compile_complex_patterns;
    (* Compilation errors *)
    test "error: unmatched paren" error_unmatched_paren;
    test "error: unmatched bracket" error_unmatched_bracket;
    test "error: invalid quantifier" error_invalid_quantifier;
    test "error: invalid escape" error_invalid_escape;
    test "error: invalid group" error_invalid_group;
    test "error: empty alternation (valid)" error_empty_alternation;
    test "error: invalid range" error_invalid_range;
    test "error: invalid backreference" error_invalid_backreference;
    (* Edge cases *)
    test "edge: nested groups" edge_nested_groups;
    test "edge: many alternatives" edge_many_alternatives;
    test "edge: long pattern" edge_long_pattern;
    test "edge: unicode pattern" edge_unicode_pattern;
    test "edge: escape sequences" edge_escape_sequences;
    test "edge: character class escapes" edge_character_class_escapes;
    test "edge: quantifier variations" edge_quantifier_variations;
    test "edge: lookaround" edge_lookaround;
    test "edge: non-capturing group" edge_non_capturing_group;
    test "edge: named groups" edge_named_groups;
    test "edge: word boundaries" edge_word_boundaries;
    test "edge: anchors" edge_anchors;
  ]
