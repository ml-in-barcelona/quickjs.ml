(* TC39 Test262: Unicode property tests

   Based on Unicode Standard and ECMAScript Unicode property requirements

   Tests for Unicode character property functions:
   - is_cased
   - is_id_start
   - is_id_continue
   - is_whitespace
   - canonicalize *)

module Unicode = Quickjs.Unicode

(* ===================================================================
   Unicode.is_cased - characters with case distinctions
   =================================================================== *)

let is_cased_ascii_letters () =
  assert_bool (Unicode.is_cased (Uchar.of_char 'a')) true;
  assert_bool (Unicode.is_cased (Uchar.of_char 'A')) true;
  assert_bool (Unicode.is_cased (Uchar.of_char 'z')) true;
  assert_bool (Unicode.is_cased (Uchar.of_char 'Z')) true

let is_cased_non_letters () =
  assert_bool (Unicode.is_cased (Uchar.of_char '0')) false;
  assert_bool (Unicode.is_cased (Uchar.of_char '!')) false;
  assert_bool (Unicode.is_cased (Uchar.of_char ' ')) false

let is_cased_accented () =
  (* é = U+00E9 *)
  assert_bool (Unicode.is_cased (Uchar.of_int 0x00E9)) true;
  (* É = U+00C9 *)
  assert_bool (Unicode.is_cased (Uchar.of_int 0x00C9)) true

(* ===================================================================
   Unicode.is_case_ignorable - characters ignored during case mapping
   =================================================================== *)

let is_case_ignorable_combining_marks () =
  (* Combining acute accent = U+0301 *)
  assert_bool (Unicode.is_case_ignorable (Uchar.of_int 0x0301)) true;
  (* Combining grave accent = U+0300 *)
  assert_bool (Unicode.is_case_ignorable (Uchar.of_int 0x0300)) true

let is_case_ignorable_letters () =
  (* Regular letters are NOT case ignorable *)
  assert_bool (Unicode.is_case_ignorable (Uchar.of_char 'a')) false;
  assert_bool (Unicode.is_case_ignorable (Uchar.of_char 'A')) false

let is_case_ignorable_apostrophe () =
  (* Apostrophe and similar characters *)
  assert_bool (Unicode.is_case_ignorable (Uchar.of_int 0x0027)) true;
  (* Right single quotation mark = U+2019 *)
  assert_bool (Unicode.is_case_ignorable (Uchar.of_int 0x2019)) true

(* ===================================================================
   Unicode.is_id_start - valid identifier start characters
   =================================================================== *)

let is_id_start_letters () =
  assert_bool (Unicode.is_id_start (Uchar.of_char 'a')) true;
  assert_bool (Unicode.is_id_start (Uchar.of_char 'Z')) true

let is_id_start_digits () =
  assert_bool (Unicode.is_id_start (Uchar.of_char '0')) false;
  assert_bool (Unicode.is_id_start (Uchar.of_char '9')) false

let is_id_start_special () =
  (* Unicode ID_Start does NOT include _ and $ - JavaScript adds them as special cases *)
  assert_bool (Unicode.is_id_start (Uchar.of_char '_')) false;
  assert_bool (Unicode.is_id_start (Uchar.of_char '$')) false

let is_id_start_unicode () =
  (* Greek alpha = U+03B1 *)
  assert_bool (Unicode.is_id_start (Uchar.of_int 0x03B1)) true;
  (* Chinese character 中 = U+4E2D *)
  assert_bool (Unicode.is_id_start (Uchar.of_int 0x4E2D)) true

(* ===================================================================
   Unicode.is_id_continue - valid identifier continuation characters
   =================================================================== *)

let is_id_continue_letters () =
  assert_bool (Unicode.is_id_continue (Uchar.of_char 'a')) true;
  assert_bool (Unicode.is_id_continue (Uchar.of_char 'Z')) true

let is_id_continue_digits () =
  assert_bool (Unicode.is_id_continue (Uchar.of_char '0')) true;
  assert_bool (Unicode.is_id_continue (Uchar.of_char '9')) true

let is_id_continue_special () =
  (* Underscore is in Unicode ID_Continue (via Pc category) *)
  assert_bool (Unicode.is_id_continue (Uchar.of_char '_')) true;
  (* Dollar is NOT in Unicode ID_Continue - JavaScript adds it *)
  assert_bool (Unicode.is_id_continue (Uchar.of_char '$')) false

(* ===================================================================
   Unicode.is_whitespace - whitespace characters
   =================================================================== *)

let is_whitespace_ascii () =
  assert_bool (Unicode.is_whitespace (Uchar.of_char ' ')) true;
  assert_bool (Unicode.is_whitespace (Uchar.of_char '\t')) true;
  assert_bool (Unicode.is_whitespace (Uchar.of_char '\n')) true

let is_whitespace_non_whitespace () =
  assert_bool (Unicode.is_whitespace (Uchar.of_char 'a')) false;
  assert_bool (Unicode.is_whitespace (Uchar.of_char '0')) false

let is_whitespace_unicode () =
  (* EN QUAD = U+2000 *)
  assert_bool (Unicode.is_whitespace (Uchar.of_int 0x2000)) true;
  (* IDEOGRAPHIC SPACE = U+3000 *)
  assert_bool (Unicode.is_whitespace (Uchar.of_int 0x3000)) true

(* ===================================================================
   Unicode.canonicalize - case folding for regex matching
   =================================================================== *)

let canonicalize_unicode_mode () =
  (* In unicode mode, uppercase converts to lowercase *)
  let result = Unicode.canonicalize (Uchar.of_char 'A') in
  assert_uchar result (Uchar.of_char 'a')

let canonicalize_lowercase_unchanged () =
  let result = Unicode.canonicalize (Uchar.of_char 'a') in
  assert_uchar result (Uchar.of_char 'a')

let canonicalize_non_unicode_mode () =
  (* In non-unicode mode, lowercase converts to uppercase (legacy behavior) *)
  let result = Unicode.canonicalize ~unicode:false (Uchar.of_char 'a') in
  assert_uchar result (Uchar.of_char 'A')

(* ===================================================================
   Unicode.fold_case - full case folding
   =================================================================== *)

let fold_case_ascii () =
  assert_string (Unicode.fold_case "Hello World") "hello world";
  assert_string (Unicode.fold_case "ABC") (Unicode.fold_case "abc")

let fold_case_sharp_s () =
  (* Full case folding expands ß (and ẞ) to "ss" *)
  assert_string (Unicode.fold_case "Straße") "strasse";
  assert_string (Unicode.fold_case "STRASSE") "strasse";
  (* U+1E9E LATIN CAPITAL LETTER SHARP S *)
  assert_string (Unicode.fold_case "\u{1E9E}") "ss"

let fold_case_sigma () =
  (* Folding is context-independent: both medial σ and final ς fold to σ,
     so the foldings of "ΣΤΙΓΜΑΣ" and "στιγμας" are equal *)
  assert_string (Unicode.fold_case "ΣΤΙΓΜΑΣ") (Unicode.fold_case "στιγμας");
  assert_string (Unicode.fold_case "ς") (Unicode.fold_case "σ")

let fold_case_kelvin () =
  (* U+212A KELVIN SIGN folds to k *)
  assert_string (Unicode.fold_case "\u{212A}") "k"

let fold_case_prosgegrammeni () =
  (* U+1F88 GREEK CAPITAL ALPHA WITH PSILI AND PROSGEGRAMMENI folds to
     U+1F00 U+03B9 (full folding), and equals the folding of its lowercase
     form U+1F80 *)
  assert_string (Unicode.fold_case "\u{1F88}") "\u{1F00}\u{03B9}";
  assert_string (Unicode.fold_case "\u{1F88}") (Unicode.fold_case "\u{1F80}")

let fold_case_idempotent () =
  (* toCasefold(toCasefold(X)) = toCasefold(X), including the capitals
     QuickJS's table folds through their lowercase form *)
  List.iter
    (fun s ->
      let once = Unicode.fold_case s in
      assert_string (Unicode.fold_case once) once)
    [ "\u{1E9E}"; "\u{1F88}"; "\u{1FBC}"; "\u{1FCC}"; "\u{1FFC}"; "Straße" ]

let fold_case_cherokee () =
  (* Cherokee folds to the uppercase letters: U+AB70 SMALL A -> U+13A0 *)
  assert_string (Unicode.fold_case "\u{AB70}") "\u{13A0}";
  assert_string (Unicode.fold_case "\u{13A0}") "\u{13A0}"

let fold_case_char_expansion () =
  (* ß folds to two code points *)
  let folded = Unicode.fold_case_char (Uchar.of_int 0x00DF) in
  Alcotest.(check (list int))
    "ß folds to ss"
    [ Char.code 's'; Char.code 's' ]
    (List.map Uchar.to_int folded);
  (* plain letters fold to themselves in one code point *)
  Alcotest.(check (list int))
    "A folds to a"
    [ Char.code 'a' ]
    (List.map Uchar.to_int (Unicode.fold_case_char (Uchar.of_char 'A')))

(* ===================================================================
   Unicode character sets - Script / General_Category / binary properties
   (the tables behind JavaScript's \p{...})
   =================================================================== *)

let char_set_exn name = function
  | Some set -> set
  | None -> Alcotest.fail (Printf.sprintf "%s: expected a char set" name)

let script_lookup () =
  let greek = char_set_exn "Greek" (Unicode.script "Greek") in
  (* α GREEK SMALL LETTER ALPHA *)
  assert_bool (Unicode.CharSet.mem (Uchar.of_int 0x03B1) greek) true;
  (* U+1F00 GREEK SMALL LETTER ALPHA WITH PSILI (Extended Greek block) *)
  assert_bool (Unicode.CharSet.mem (Uchar.of_int 0x1F00) greek) true;
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'a') greek) false

let script_short_name_alias () =
  (* \p{Script=Grek} and \p{Script=Greek} are the same set *)
  let long = char_set_exn "Greek" (Unicode.script "Greek") in
  let short = char_set_exn "Grek" (Unicode.script "Grek") in
  Alcotest.(check (array (pair int int)))
    "long and short names give the same ranges"
    (Unicode.CharSet.ranges long)
    (Unicode.CharSet.ranges short)

let script_extensions () =
  (* U+00B7 MIDDLE DOT: Script=Common, but Greek is in Script_Extensions *)
  let middle_dot = Uchar.of_int 0x00B7 in
  let script_only = char_set_exn "Greek" (Unicode.script "Greek") in
  let with_ext =
    char_set_exn "Greek ext" (Unicode.script ~extensions:true "Greek")
  in
  assert_bool (Unicode.CharSet.mem middle_dot script_only) false;
  assert_bool (Unicode.CharSet.mem middle_dot with_ext) true

let script_unknown () =
  (match Unicode.script "NotAScript" with
  | None -> ()
  | Some _ -> Alcotest.fail "unknown script must be None");
  (* names are case-sensitive, like JavaScript's \p{...} *)
  match Unicode.script "greek" with
  | None -> ()
  | Some _ -> Alcotest.fail "script names are case-sensitive"

let general_category_lookup () =
  let upper = char_set_exn "Lu" (Unicode.general_category "Lu") in
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'A') upper) true;
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'a') upper) false;
  (* É *)
  assert_bool (Unicode.CharSet.mem (Uchar.of_int 0x00C9) upper) true

let general_category_aliases () =
  (* \p{Lu} = \p{Uppercase_Letter} *)
  let short = char_set_exn "Lu" (Unicode.general_category "Lu") in
  let long =
    char_set_exn "Uppercase_Letter"
      (Unicode.general_category "Uppercase_Letter")
  in
  Alcotest.(check (array (pair int int)))
    "short and long names give the same ranges"
    (Unicode.CharSet.ranges short)
    (Unicode.CharSet.ranges long)

let general_category_grouped () =
  (* \p{L} groups Lu|Ll|Lt|Lm|Lo *)
  let letter = char_set_exn "L" (Unicode.general_category "L") in
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'A') letter) true;
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'a') letter) true;
  (* 中 *)
  assert_bool (Unicode.CharSet.mem (Uchar.of_int 0x4E2D) letter) true;
  assert_bool (Unicode.CharSet.mem (Uchar.of_char '0') letter) false

let general_category_surrogates () =
  (* \p{Cs}: ranges may contain surrogate code points, hence int ranges *)
  let surrogates = char_set_exn "Cs" (Unicode.general_category "Cs") in
  Alcotest.(check (array (pair int int)))
    "Cs is exactly the surrogate range"
    [| (0xD800, 0xDFFF) |]
    (Unicode.CharSet.ranges surrogates)

let general_category_unknown () =
  match Unicode.general_category "Xx" with
  | None -> ()
  | Some _ -> Alcotest.fail "unknown category must be None"

let binary_property_lookup () =
  let alpha =
    char_set_exn "Alphabetic" (Unicode.binary_property "Alphabetic")
  in
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'a') alpha) true;
  assert_bool (Unicode.CharSet.mem (Uchar.of_char '1') alpha) false;
  let ws = char_set_exn "White_Space" (Unicode.binary_property "White_Space") in
  assert_bool (Unicode.CharSet.mem (Uchar.of_char ' ') ws) true;
  (* U+00A0 NO-BREAK SPACE *)
  assert_bool (Unicode.CharSet.mem (Uchar.of_int 0x00A0) ws) true;
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'a') ws) false

let binary_property_emoji () =
  let emoji = char_set_exn "Emoji" (Unicode.binary_property "Emoji") in
  (* 😀 U+1F600 *)
  assert_bool (Unicode.CharSet.mem (Uchar.of_int 0x1F600) emoji) true;
  assert_bool (Unicode.CharSet.mem (Uchar.of_char 'a') emoji) false

let binary_property_unknown () =
  match Unicode.binary_property "NotAProperty" with
  | None -> ()
  | Some _ -> Alcotest.fail "unknown property must be None"

let char_set_ranges_sorted_disjoint () =
  let greek = char_set_exn "Greek" (Unicode.script "Greek") in
  let ranges = Unicode.CharSet.ranges greek in
  assert_bool (Array.length ranges > 0) true;
  Array.iteri
    (fun i (first, last) ->
      assert_bool (first <= last) true;
      if i > 0 then
        let _, prev_last = ranges.(i - 1) in
        (* strictly increasing and disjoint *)
        assert_bool (prev_last < first) true)
    ranges

let tests =
  [
    (* is_cased *)
    test "is_cased: ASCII letters" is_cased_ascii_letters;
    test "is_cased: non-letters" is_cased_non_letters;
    test "is_cased: accented" is_cased_accented;
    (* is_case_ignorable *)
    test "is_case_ignorable: combining marks" is_case_ignorable_combining_marks;
    test "is_case_ignorable: letters" is_case_ignorable_letters;
    test "is_case_ignorable: apostrophe" is_case_ignorable_apostrophe;
    (* is_id_start *)
    test "is_id_start: letters" is_id_start_letters;
    test "is_id_start: digits" is_id_start_digits;
    test "is_id_start: special chars" is_id_start_special;
    test "is_id_start: unicode" is_id_start_unicode;
    (* is_id_continue *)
    test "is_id_continue: letters" is_id_continue_letters;
    test "is_id_continue: digits" is_id_continue_digits;
    test "is_id_continue: special" is_id_continue_special;
    (* is_whitespace *)
    test "is_whitespace: ASCII" is_whitespace_ascii;
    test "is_whitespace: non-whitespace" is_whitespace_non_whitespace;
    test "is_whitespace: unicode" is_whitespace_unicode;
    (* canonicalize *)
    test "canonicalize: unicode mode" canonicalize_unicode_mode;
    test "canonicalize: lowercase unchanged" canonicalize_lowercase_unchanged;
    test "canonicalize: non-unicode mode" canonicalize_non_unicode_mode;
    (* fold_case *)
    test "fold_case: ASCII" fold_case_ascii;
    test "fold_case: sharp s expands" fold_case_sharp_s;
    test "fold_case: sigma is context-independent" fold_case_sigma;
    test "fold_case: Kelvin sign" fold_case_kelvin;
    test "fold_case: prosgegrammeni full folding" fold_case_prosgegrammeni;
    test "fold_case: idempotent" fold_case_idempotent;
    test "fold_case: Cherokee folds to uppercase" fold_case_cherokee;
    test "fold_case_char: expansion" fold_case_char_expansion;
    (* character sets *)
    test "script: lookup and membership" script_lookup;
    test "script: short name alias" script_short_name_alias;
    test "script: extensions" script_extensions;
    test "script: unknown and case-sensitivity" script_unknown;
    test "general_category: lookup" general_category_lookup;
    test "general_category: aliases" general_category_aliases;
    test "general_category: grouped L" general_category_grouped;
    test "general_category: surrogates in Cs" general_category_surrogates;
    test "general_category: unknown" general_category_unknown;
    test "binary_property: Alphabetic and White_Space" binary_property_lookup;
    test "binary_property: Emoji" binary_property_emoji;
    test "binary_property: unknown" binary_property_unknown;
    test "char set ranges are sorted and disjoint"
      char_set_ranges_sorted_disjoint;
  ]
