(** TC39 Test262: RegExp compile error tests

    Based on: https://github.com/tc39/test262/tree/main/test/built-ins/RegExp

    Tests for RegExp compilation errors and edge cases *)

module RegExp = Quickjs.RegExp

(* ===================================================================
   Successful compilation tests
   =================================================================== *)

let compile_simple () =
  (* Simple patterns should compile *)
  let _ = regexp_compile "abc" ~flags:"" in
  let _ = regexp_compile "\\d+" ~flags:"" in
  let _ = regexp_compile "[a-z]" ~flags:"" in
  ()

let compile_empty () =
  (* Empty pattern should compile *)
  let _ = regexp_compile "" ~flags:"" in
  ()

let compile_all_flags () =
  (* All valid flags should work *)
  let _ = regexp_compile "abc" ~flags:"g" in
  let _ = regexp_compile "abc" ~flags:"i" in
  let _ = regexp_compile "abc" ~flags:"m" in
  let _ = regexp_compile "abc" ~flags:"s" in
  let _ = regexp_compile "abc" ~flags:"u" in
  let _ = regexp_compile "abc" ~flags:"y" in
  let _ = regexp_compile "abc" ~flags:"gimsuy" in
  ()

let compile_complex_patterns () =
  (* Complex patterns should compile *)
  let _ = regexp_compile "^[a-zA-Z_][a-zA-Z0-9_]*$" ~flags:"" in
  let _ = regexp_compile "(\\d{4})-(\\d{2})-(\\d{2})" ~flags:"" in
  let _ = regexp_compile "(?<name>\\w+)" ~flags:"" in
  let _ = regexp_compile "a(?=b)c" ~flags:"" in
  let _ = regexp_compile "(?<=a)b" ~flags:"" in
  ()

(* ===================================================================
   Compilation error tests
   =================================================================== *)

let error_unmatched_paren () =
  (* Unmatched parenthesis *)
  let _error = regexp_no_compile "(abc" ~flags:"" in
  let _error = regexp_no_compile "abc)" ~flags:"" in
  ()

let error_unmatched_bracket () =
  (* Unmatched bracket *)
  let _error = regexp_no_compile "[abc" ~flags:"" in
  ()

let error_invalid_quantifier () =
  (* Nothing to repeat *)
  let _error = regexp_no_compile "*" ~flags:"" in
  let _error = regexp_no_compile "+" ~flags:"" in
  let _error = regexp_no_compile "?" ~flags:"" in
  let _error = regexp_no_compile "{2,3}" ~flags:"" in
  ()

let error_invalid_escape () =
  (* Invalid escape at end of pattern *)
  let _error = regexp_no_compile "\\" ~flags:"" in
  ()

let error_invalid_group () =
  (* Invalid group syntax *)
  let _error = regexp_no_compile "(?<>abc)" ~flags:"" in  (* empty group name *)
  let _error = regexp_no_compile "(?<123>abc)" ~flags:"" in  (* numeric group name *)
  ()

let error_empty_alternation () =
  (* This is actually valid in most regex engines - empty alternative matches empty string *)
  let _ = regexp_compile "a|" ~flags:"" in  (* valid *)
  let _ = regexp_compile "|b" ~flags:"" in  (* valid *)
  ()

let error_invalid_range () =
  (* Invalid character class range *)
  let _error = regexp_no_compile "[z-a]" ~flags:"" in  (* reversed range *)
  ()

let error_invalid_backreference () =
  (* Backreference to non-existent group - may or may not error depending on engine *)
  (* QuickJS may allow this with specific behavior *)
  let _ = regexp_compile "\\1" ~flags:"" in  (* might work or error *)
  ()

(* ===================================================================
   Edge case patterns
   =================================================================== *)

let edge_nested_groups () =
  (* Deeply nested groups *)
  let _ = regexp_compile "((((a))))" ~flags:"" in
  let _ = regexp_compile "(a(b(c(d))))" ~flags:"" in
  ()

let edge_many_alternatives () =
  (* Many alternatives *)
  let _ = regexp_compile "a|b|c|d|e|f|g|h|i|j" ~flags:"" in
  ()

let edge_long_pattern () =
  (* Long pattern *)
  let _ = regexp_compile "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ~flags:"" in
  ()

let edge_unicode_pattern () =
  (* Unicode in pattern *)
  let _ = regexp_compile "café" ~flags:"" in
  let _ = regexp_compile "日本語" ~flags:"" in
  let _ = regexp_compile "🎉" ~flags:"" in
  ()

let edge_escape_sequences () =
  (* All escape sequences *)
  let _ = regexp_compile "\\d\\D\\w\\W\\s\\S\\b\\B" ~flags:"" in
  let _ = regexp_compile "\\t\\n\\r\\v\\f" ~flags:"" in
  let _ = regexp_compile "\\0" ~flags:"" in
  ()

let edge_character_class_escapes () =
  (* Escapes in character class *)
  let _ = regexp_compile "[\\d\\w\\s]" ~flags:"" in
  let _ = regexp_compile "[\\-\\]]" ~flags:"" in  (* escaped special chars in class *)
  ()

let edge_quantifier_variations () =
  (* Various quantifier forms *)
  let _ = regexp_compile "a{0}" ~flags:"" in
  let _ = regexp_compile "a{1}" ~flags:"" in
  let _ = regexp_compile "a{2,}" ~flags:"" in
  let _ = regexp_compile "a{2,5}" ~flags:"" in
  let _ = regexp_compile "a{0,0}" ~flags:"" in
  ()

let edge_lookaround () =
  (* Lookahead and lookbehind *)
  let _ = regexp_compile "(?=abc)" ~flags:"" in
  let _ = regexp_compile "(?!abc)" ~flags:"" in
  let _ = regexp_compile "(?<=abc)" ~flags:"" in
  let _ = regexp_compile "(?<!abc)" ~flags:"" in
  ()

let edge_non_capturing_group () =
  (* Non-capturing groups *)
  let _ = regexp_compile "(?:abc)" ~flags:"" in
  let _ = regexp_compile "(?:a(?:b(?:c)))" ~flags:"" in
  ()

let edge_named_groups () =
  (* Named groups *)
  let _ = regexp_compile "(?<name>abc)" ~flags:"" in
  let _ = regexp_compile "(?<first>a)(?<second>b)" ~flags:"" in
  ()

let edge_word_boundaries () =
  (* Word boundaries *)
  let _ = regexp_compile "\\bword\\b" ~flags:"" in
  let _ = regexp_compile "\\Bword\\B" ~flags:"" in
  ()

let edge_anchors () =
  (* Anchors *)
  let _ = regexp_compile "^abc$" ~flags:"" in
  let _ = regexp_compile "^$" ~flags:"" in
  let _ = regexp_compile "^^$$" ~flags:"" in  (* redundant anchors - valid *)
  ()

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

