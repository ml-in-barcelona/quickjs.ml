(* Regression tests for the 2026-07 audit findings.

   Each section references the behavior that used to be broken. These tests
   pin the fixed semantics so they cannot silently regress. *)

module RegExp = Quickjs.RegExp
module String = Quickjs.String
module Number = Quickjs.Number
module Global = Quickjs.Global

(* ===================================================================
   lastIndex bookkeeping (used to be overwritten by every capture group)
   =================================================================== *)

let last_index_from_full_match () =
  (* (a)b on "abab": group 1 ends at 1 but the match ends at 2 *)
  let re = regexp_compile "(a)b" ~flags:"g" in
  let result = RegExp.exec re "abab" in
  assert_match result [| "ab"; "a" |];
  assert_int (RegExp.last_index re) 2;
  let result = RegExp.exec re "abab" in
  assert_match_index result 2;
  assert_int (RegExp.last_index re) 4;
  assert_no_match (RegExp.exec re "abab")

let no_overlapping_rematch () =
  (* (ab)a on "ababa": JS finds exactly one match (lastIndex = 3) *)
  let matches = String.Prototype.match_global "(ab)a" "ababa" in
  assert_array matches [| "aba" |]

let zero_width_group_no_infinite_loop () =
  (* (x?)ab: zero-width group at match start used to reset lastIndex to the
     match start and loop forever *)
  let matches = String.Prototype.match_global "(x?)ab" "abab" in
  assert_array matches [| "ab"; "ab" |]

let non_global_does_not_touch_last_index () =
  let re = regexp_compile "b" ~flags:"" in
  let result = RegExp.exec re "abc" in
  assert_match result [| "b" |];
  assert_int (RegExp.last_index re) 0

(* ===================================================================
   Non-participating capture groups (used to produce garbage pointers)
   =================================================================== *)

let trailing_non_participating_group () =
  (* (b)|(z) on "b": the last group does not participate. lastIndex used to
     become a negated heap address and hang the next exec *)
  let re = regexp_compile "(b)|(z)" ~flags:"g" in
  let result = RegExp.exec re "b" in
  assert_captures result [| Some "b"; Some "b"; None |];
  assert_int (RegExp.last_index re) 1;
  (* second exec must terminate and find nothing *)
  assert_no_match (RegExp.exec re "b")

let non_participating_named_group () =
  let re = regexp_compile "(?<a>x)?(?<b>y)" ~flags:"" in
  let result = RegExp.exec re "y" in
  (* None (JS undefined) is distinct from a group matching "" *)
  assert_captures result [| Some "y"; None; Some "y" |];
  assert_group result "a" None;
  assert_group result "b" (Some "y")

let set_last_index_clamps_negative () =
  let re = regexp_compile "a" ~flags:"g" in
  RegExp.set_last_index re (-5);
  assert_int (RegExp.last_index re) 0;
  assert_match (RegExp.exec re "aaa") [| "a" |]

(* ===================================================================
   Flags (used to be silently ignored / mutated)
   =================================================================== *)

let unknown_flag_rejected () =
  assert_invalid_flags (regexp_no_compile "a" ~flags:"q")

let duplicated_flag_rejected () =
  assert_invalid_flags (regexp_no_compile "a" ~flags:"gg")

let u_and_v_rejected () =
  assert_invalid_flags (regexp_no_compile "a" ~flags:"uv")

let all_flags_accepted_and_canonical () =
  let re = regexp_compile "a" ~flags:"ysmigd" in
  (* canonical "dgimsuvy" order, exactly the flags that were passed *)
  assert_string (RegExp.flags re) "dgimsy";
  assert_bool (RegExp.indices re) true;
  assert_bool (RegExp.global re) true

let unicode_sets_flag () =
  let re = regexp_compile "a" ~flags:"v" in
  assert_string (RegExp.flags re) "v";
  assert_bool (RegExp.unicode_sets re) true;
  assert_bool (RegExp.unicode re) false;
  assert_bool (RegExp.test re "a") true

let non_ascii_pattern_keeps_flags () =
  (* compiling a non-ASCII pattern must not invent a 'u' flag *)
  let re = regexp_compile "café" ~flags:"" in
  assert_string (RegExp.flags re) "";
  assert_bool (RegExp.unicode re) false;
  assert_bool (RegExp.test re "un café") true

let astral_pattern_without_u () =
  (* Astral characters compile as surrogate pairs in non-unicode mode
     (CESU-8), exactly like JavaScript *)
  let re = regexp_compile "😀" ~flags:"" in
  assert_string (RegExp.flags re) "";
  let result = RegExp.exec re "a😀b" in
  assert_match result [| "😀" |];
  (* index is in UTF-16 code units *)
  assert_match_index result 1

(* ===================================================================
   UTF-16 index units across RegExp and String (used to leak UTF-8 bytes)
   =================================================================== *)

let exec_index_is_utf16 () =
  let re = regexp_compile "b" ~flags:"" in
  let result = RegExp.exec re "ébé" in
  (* é is 2 UTF-8 bytes but 1 UTF-16 unit *)
  assert_match_index result 1

let last_index_is_utf16 () =
  let re = regexp_compile "é" ~flags:"g" in
  let _ = RegExp.exec re "éé" in
  assert_int (RegExp.last_index re) 1;
  let result = RegExp.exec re "éé" in
  assert_match_index result 1

let split_regex_unicode () =
  let parts = String.Prototype.split_regex "b" "ébé" in
  assert_array parts [| "é"; "é" |]

let search_unicode () = assert_int (String.Prototype.search "b" "ébé") 1

let slice_splits_surrogate_pair () =
  (* "😀".slice(0, 1) yields the lone high surrogate in JavaScript; UTF-8
     cannot represent it, so it becomes U+FFFD. Used to raise
     Invalid_argument ("D83D is not a Unicode scalar value"). *)
  let high = String.Prototype.slice ~start:0 ~end_:1 "😀" in
  assert_string high "\u{FFFD}";
  assert_int (String.Prototype.length high) 1;
  assert_string (String.Prototype.slice ~start:1 ~end_:2 "😀") "\u{FFFD}";
  assert_string (String.Prototype.substring ~start:0 ~end_:1 "😀") "\u{FFFD}";
  assert_string (String.Prototype.substr ~start:0 ~length:1 "😀") "\u{FFFD}"

(* ===================================================================
   split_regex spec semantics
   =================================================================== *)

let split_regex_empty_pattern () =
  (* "ab".split(/(?:)/) is ["a"; "b"] per the spec's SplitMatch loop *)
  assert_array (String.Prototype.split_regex "(?:)" "ab") [| "a"; "b" |]

let split_regex_empty_input () =
  (* splitting "" yields [] when the separator matches it, [""] otherwise *)
  assert_array (String.Prototype.split_regex "(?:)" "") [||];
  assert_array (String.Prototype.split_regex "x" "") [| "" |]

let split_regex_captures () =
  assert_array (String.Prototype.split_regex "(\\d)" "a1b") [| "a"; "1"; "b" |]

(* ===================================================================
   Replacement semantics ($` used to see already-replaced text)
   =================================================================== *)

let replace_all_dollar_backtick () =
  (* $` refers to the original string before this match *)
  assert_string (String.Prototype.replace_all "a" "[$`]" "aa") "[][a]"

let replace_all_dollar_quote () =
  (* $' refers to the original string after this match *)
  assert_string (String.Prototype.replace_all "a" "[$']" "aa") "[a][]"

let replace_regex_global_dollar_backtick () =
  assert_string (String.Prototype.replace_regex_global "a" "[$`]" "aa") "[][a]"

(* ===================================================================
   Invalid patterns raise instead of silently returning "no match"
   =================================================================== *)

let invalid_pattern_raises () =
  let expect_invalid f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail "Expected Invalid_argument"
  in
  expect_invalid (fun () -> String.Prototype.search "(" "abc");
  expect_invalid (fun () -> String.Prototype.replace_regex "(" "x" "abc");
  expect_invalid (fun () -> String.Prototype.split_regex "(" "abc");
  expect_invalid (fun () -> String.Prototype.match_global "(" "abc")

(* ===================================================================
   Whitespace (single source of truth, includes U+1680)
   =================================================================== *)

let trim_ogham_space () =
  (* U+1680 OGHAM SPACE MARK is JavaScript whitespace *)
  assert_string (String.Prototype.trim "\xe1\x9a\x80x\xe1\x9a\x80") "x";
  assert_string (String.Prototype.trim_start "\xe1\x9a\x80x") "x";
  assert_string (String.Prototype.trim_end "x\xe1\x9a\x80") "x"

(* ===================================================================
   Final sigma (context-sensitive lowercase)
   =================================================================== *)

let final_sigma () =
  (* Σ at the end of a word lowercases to ς, elsewhere to σ *)
  assert_string (String.Prototype.to_lower_case "ΑΣ") "ας";
  assert_string (String.Prototype.to_lower_case "ΣΑ") "σα";
  assert_string (String.Prototype.to_lower_case "Σ") "σ";
  assert_string (String.Prototype.to_lower_case "ΟΔΥΣΣΕΥΣ") "οδυσσευς"

(* ===================================================================
   starts_with_from overflow
   =================================================================== *)

let starts_with_from_huge_pos () =
  assert_bool (String.Prototype.starts_with_from "a" max_int "abc") false;
  assert_bool (String.Prototype.starts_with_from "" max_int "abc") true

(* ===================================================================
   parse_int (used to mis-parse 0x, corrupt huge values)
   =================================================================== *)

let parse_int_auto_detects_hex () =
  (* JavaScript's parseInt default radix auto-detects 0x *)
  assert_int_opt (Global.parse_int "0x10") (Some 16);
  assert_int_opt (Global.parse_int ~radix:16 "0x10") (Some 16);
  (* but with an explicit radix other than 16, 0x is not special *)
  assert_int_opt (Global.parse_int ~radix:10 "0x10") (Some 0)

let parse_int_no_binary_octal () =
  (* parseInt never auto-detects 0b/0o (unlike Number()) *)
  assert_int_opt (Global.parse_int "0b1") (Some 0);
  assert_int_opt (Global.parse_int "0o7") (Some 0)

let parse_int_out_of_range () =
  (* values that do not fit in an OCaml int are refused, not corrupted *)
  assert_int_opt (Global.parse_int "99999999999999999999999999") None;
  assert_int_opt (Global.parse_int "-99999999999999999999999999") None

let parse_int_unicode_whitespace () =
  (* NBSP before the number *)
  assert_int_opt (Global.parse_int "\xc2\xa042") (Some 42)

let parse_int_float_js_codomain () =
  (* parse_int_float follows JavaScript exactly: huge values come back as
     lossy floats instead of None *)
  assert_float_opt
    (Global.parse_int_float "99999999999999999999999999")
    (Some 1e26);
  assert_float_opt (Global.parse_int_float "0x10") (Some 16.0);
  assert_float_opt (Global.parse_int_float "abc") None

(* ===================================================================
   Index unit conversions (for byte-oriented consumers)
   =================================================================== *)

let index_unit_conversions () =
  (* "ébé": é = 2 bytes / 1 UTF-16 unit *)
  let s = "\xc3\xa9b\xc3\xa9" in
  assert_int (String.utf16_index_of_byte s 0) 0;
  assert_int (String.utf16_index_of_byte s 2) 1;
  assert_int (String.utf16_index_of_byte s 3) 2;
  assert_int (String.utf16_index_of_byte s 100) 3;
  assert_int (String.byte_index_of_utf16 s 0) 0;
  assert_int (String.byte_index_of_utf16 s 1) 2;
  assert_int (String.byte_index_of_utf16 s 2) 3;
  assert_int (String.byte_index_of_utf16 s 100) 5;
  (* astral char: 😀 = 4 bytes / 2 UTF-16 units *)
  let e = "a\xf0\x9f\x98\x80b" in
  assert_int (String.byte_index_of_utf16 e 1) 1;
  assert_int (String.byte_index_of_utf16 e 3) 5;
  assert_int (String.utf16_index_of_byte e 5) 3;
  (* round-trip a RegExp match index into a byte offset for Stdlib slicing *)
  let re = regexp_compile "b" ~flags:"" in
  match RegExp.exec re s with
  | Some m ->
      let byte_idx = String.byte_index_of_utf16 s m.RegExp.index in
      assert_string (Stdlib.String.sub s 0 byte_idx) "\xc3\xa9"
  | None -> Alcotest.fail "expected a match"

(* ===================================================================
   Number formatting validation (used to abort the process)
   =================================================================== *)

let fixed_zero_raises () =
  match
    Number.Prototype.to_string
      ~options:{ Number.default_options with format = Number.Fixed 0 }
      3.14
  with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "Expected Invalid_argument for Fixed 0"

let radix_with_fixed_raises () =
  match
    Number.Prototype.to_string
      ~options:
        {
          Number.format = Number.Fixed 5;
          exponent = Number.Auto;
          radix = 16;
          show_minus_zero = false;
        }
      255.5
  with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "Expected Invalid_argument for radix 16 + Fixed"

let js_digit_ranges () =
  (* JavaScript's RangeError limits: 0-100 *)
  assert_string
    (Number.Prototype.to_exponential 100 1.0)
    (Number.Prototype.to_exponential 100 1.0);
  (match Number.Prototype.to_exponential 101 1.0 with
  | exception Invalid_argument msg ->
      assert_bool
        (string_contains ~needle:"to_exponential" msg
        && string_contains ~needle:"101" msg)
        true
  | _ -> Alcotest.fail "Expected Invalid_argument for to_exponential 101");
  match Number.Prototype.to_fixed 101 1.0 with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "Expected Invalid_argument for to_fixed 101"

(* ===================================================================
   Regexp engine safety (stack guard and timeout)
   =================================================================== *)

let deeply_nested_pattern_errors () =
  (* A pattern nested deeper than the C-stack budget must produce a compile
     error instead of smashing the process stack *)
  let depth = 500_000 in
  let pattern =
    Stdlib.String.concat ""
      [ Stdlib.String.make depth '('; "a"; Stdlib.String.make depth ')' ]
  in
  match Quickjs.RegExp.compile ~flags:"" pattern with
  | Error (`Stack_overflow | `Unknown _) -> ()
  | Error other ->
      Alcotest.fail
        (Printf.sprintf "Expected `Stack_overflow, got %s"
           (Quickjs.RegExp.compile_error_to_string other))
  | Ok _ -> Alcotest.fail "Expected a compile error for deep nesting"

let exec_timeout () =
  (* Catastrophic backtracking bounded by timeout_ms *)
  let re = regexp_compile "(a+)+$" ~flags:"" in
  let input = Stdlib.String.make 64 'a' ^ "b" in
  match RegExp.exec ~timeout_ms:10.0 re input with
  | exception RegExp.Timeout -> ()
  | Some _ -> Alcotest.fail "Expected Timeout, got a match"
  | None -> Alcotest.fail "Expected Timeout, got no match"

let tests =
  [
    test "lastIndex: derived from full match only" last_index_from_full_match;
    test "lastIndex: no overlapping rematch" no_overlapping_rematch;
    test "lastIndex: zero-width group terminates"
      zero_width_group_no_infinite_loop;
    test "lastIndex: untouched without global/sticky"
      non_global_does_not_touch_last_index;
    test "captures: trailing non-participating group"
      trailing_non_participating_group;
    test "captures: non-participating named group" non_participating_named_group;
    test "lastIndex: negative values clamp to 0" set_last_index_clamps_negative;
    test "flags: unknown flag rejected" unknown_flag_rejected;
    test "flags: duplicated flag rejected" duplicated_flag_rejected;
    test "flags: u+v rejected" u_and_v_rejected;
    test "flags: canonical order, d flag" all_flags_accepted_and_canonical;
    test "flags: unicode sets (v)" unicode_sets_flag;
    test "flags: non-ASCII pattern keeps flags" non_ascii_pattern_keeps_flags;
    test "flags: astral pattern without u" astral_pattern_without_u;
    test "utf16: exec index" exec_index_is_utf16;
    test "utf16: lastIndex" last_index_is_utf16;
    test "utf16: split_regex" split_regex_unicode;
    test "utf16: slice splitting a surrogate pair yields U+FFFD"
      slice_splits_surrogate_pair;
    test "utf16: search" search_unicode;
    test "split_regex: empty pattern" split_regex_empty_pattern;
    test "split_regex: empty input" split_regex_empty_input;
    test "split_regex: captures spliced" split_regex_captures;
    test "replace_all: $` sees original" replace_all_dollar_backtick;
    test "replace_all: $' sees original" replace_all_dollar_quote;
    test "replace_regex_global: $` sees original"
      replace_regex_global_dollar_backtick;
    test "regex methods: invalid pattern raises" invalid_pattern_raises;
    test "trim: U+1680 ogham space" trim_ogham_space;
    test "to_lower_case: final sigma" final_sigma;
    test "starts_with_from: huge position" starts_with_from_huge_pos;
    test "parse_int: auto-detects hex" parse_int_auto_detects_hex;
    test "parse_int: no 0b/0o" parse_int_no_binary_octal;
    test "parse_int: out of int range" parse_int_out_of_range;
    test "parse_int: unicode whitespace" parse_int_unicode_whitespace;
    test "parse_int_float: JS codomain" parse_int_float_js_codomain;
    test "string: index unit conversions" index_unit_conversions;
    test "number: Fixed 0 raises" fixed_zero_raises;
    test "number: radix+Fixed raises" radix_with_fixed_raises;
    test "number: JS digit ranges" js_digit_ranges;
    test "regexp: deep nesting errors cleanly" deeply_nested_pattern_errors;
    test "regexp: exec timeout" exec_timeout;
  ]
