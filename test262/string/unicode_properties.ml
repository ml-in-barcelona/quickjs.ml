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
  ]
