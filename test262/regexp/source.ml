(* TC39 Test262: RegExp.prototype.source tests

    Based on:
    https://github.com/tc39/test262/tree/main/test/built-ins/RegExp/prototype/source

    ECMA-262 Section: RegExp.prototype.source

    The source property returns the text of the pattern. *)

module RegExp = Quickjs.RegExp

let source_simple () =
  (* Simple pattern *)
  let re = regexp_compile "abc" ~flags:"" in
  assert_string (RegExp.source re) "abc"

let source_empty () =
  (* Empty pattern - returns "(?:)" in some engines *)
  let re = regexp_compile "" ~flags:"" in
  let source = RegExp.source re in
  (* Either empty string or empty non-capturing group *)
  assert_bool (source = "" || source = "(?:)") true

let source_with_flags () =
  (* Source is independent of flags *)
  let re = regexp_compile "abc" ~flags:"gi" in
  assert_string (RegExp.source re) "abc"

let source_special_chars () =
  (* Special regex characters *)
  let re = regexp_compile "a.b" ~flags:"" in
  assert_string (RegExp.source re) "a.b";

  let re2 = regexp_compile "a*b+" ~flags:"" in
  assert_string (RegExp.source re2) "a*b+";

  let re3 = regexp_compile "^start$" ~flags:"" in
  assert_string (RegExp.source re3) "^start$"

let source_escape_sequences () =
  (* Escape sequences *)
  let re = regexp_compile "\\d+" ~flags:"" in
  assert_string (RegExp.source re) "\\d+";

  let re2 = regexp_compile "\\w\\s\\n" ~flags:"" in
  assert_string (RegExp.source re2) "\\w\\s\\n"

let source_character_class () =
  (* Character class *)
  let re = regexp_compile "[a-z]" ~flags:"" in
  assert_string (RegExp.source re) "[a-z]";

  let re2 = regexp_compile "[^0-9]" ~flags:"" in
  assert_string (RegExp.source re2) "[^0-9]"

let source_groups () =
  (* Groups *)
  let re = regexp_compile "(abc)" ~flags:"" in
  assert_string (RegExp.source re) "(abc)";

  let re2 = regexp_compile "(?:abc)" ~flags:"" in
  assert_string (RegExp.source re2) "(?:abc)";

  let re3 = regexp_compile "(?<name>abc)" ~flags:"" in
  assert_string (RegExp.source re3) "(?<name>abc)"

let source_alternation () =
  (* Alternation *)
  let re = regexp_compile "cat|dog" ~flags:"" in
  assert_string (RegExp.source re) "cat|dog"

let source_quantifiers () =
  (* Quantifiers *)
  let re = regexp_compile "a{2,4}" ~flags:"" in
  assert_string (RegExp.source re) "a{2,4}";

  let re2 = regexp_compile "a+?" ~flags:"" in
  assert_string (RegExp.source re2) "a+?"

let source_lookaround () =
  (* Lookahead and lookbehind *)
  let re = regexp_compile "a(?=b)" ~flags:"" in
  assert_string (RegExp.source re) "a(?=b)";

  let re2 = regexp_compile "a(?!b)" ~flags:"" in
  assert_string (RegExp.source re2) "a(?!b)";

  let re3 = regexp_compile "(?<=a)b" ~flags:"" in
  assert_string (RegExp.source re3) "(?<=a)b";

  let re4 = regexp_compile "(?<!a)b" ~flags:"" in
  assert_string (RegExp.source re4) "(?<!a)b"

let source_unicode () =
  (* Unicode patterns *)
  let re = regexp_compile "café" ~flags:"" in
  assert_string (RegExp.source re) "café"

let source_complex () =
  (* Complex pattern *)
  let re = regexp_compile "^[a-zA-Z_][a-zA-Z0-9_]*$" ~flags:"" in
  assert_string (RegExp.source re) "^[a-zA-Z_][a-zA-Z0-9_]*$"

let source_with_forward_slash () =
  (* Forward slash needs escaping in literal syntax but not in source *)
  let re = regexp_compile "a/b" ~flags:"" in
  assert_string (RegExp.source re) "a/b"

let source_backslash () =
  (* Backslash in pattern *)
  let re = regexp_compile "\\\\" ~flags:"" in
  (* matches a single backslash *)
  assert_string (RegExp.source re) "\\\\"

let tests =
  [
    test "source: simple pattern" source_simple;
    test "source: empty pattern" source_empty;
    test "source: independent of flags" source_with_flags;
    test "source: special chars" source_special_chars;
    test "source: escape sequences" source_escape_sequences;
    test "source: character class" source_character_class;
    test "source: groups" source_groups;
    test "source: alternation" source_alternation;
    test "source: quantifiers" source_quantifiers;
    test "source: lookaround" source_lookaround;
    test "source: unicode" source_unicode;
    test "source: complex" source_complex;
    test "source: forward slash" source_with_forward_slash;
    test "source: backslash" source_backslash;
  ]
