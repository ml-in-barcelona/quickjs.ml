(* TC39 Test262: String.prototype.charAt tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/charAt

   ECMA-262 Section: String.prototype.charAt(pos)

   Tests for String.Prototype.char_at *)

module String = Quickjs.String

(* ===================================================================
   Basic charAt functionality - S15.5.4.4
   =================================================================== *)

let basic_ascii () =
  assert_string (String.Prototype.char_at 0 "hello") "h";
  assert_string (String.Prototype.char_at 1 "hello") "e";
  assert_string (String.Prototype.char_at 4 "hello") "o"

let out_of_bounds () =
  (* Out of bounds returns empty string in JavaScript *)
  assert_string (String.Prototype.char_at (-1) "hello") "";
  assert_string (String.Prototype.char_at 5 "hello") "";
  assert_string (String.Prototype.char_at 100 "hello") ""

let empty_string () =
  assert_string (String.Prototype.char_at 0 "") ""

let unicode_bmp () =
  (* BMP characters (single UTF-16 code unit) *)
  assert_string (String.Prototype.char_at 0 "café") "c";
  assert_string (String.Prototype.char_at 3 "café") "é";
  assert_string (String.Prototype.char_at 0 "日本語") "日";
  assert_string (String.Prototype.char_at 2 "日本語") "語"

let unicode_surrogate_pairs () =
  (* Emoji and other characters outside BMP (surrogate pairs in UTF-16) *)
  (* "😀" is U+1F600, represented as surrogate pair in UTF-16 *)
  (* charAt returns the high surrogate at index 0, low surrogate at index 1 *)
  let emoji = "😀" in
  let result0 = String.Prototype.char_at 0 emoji in
  let result1 = String.Prototype.char_at 1 emoji in
  (* In JS semantics, these would be the individual surrogates *)
  assert_bool (Stdlib.String.length result0 > 0) true;
  assert_bool (Stdlib.String.length result1 > 0) true

let mixed_content () =
  (* String with mixed ASCII, BMP, and surrogate pairs *)
  let s = "a日😀b" in
  assert_string (String.Prototype.char_at 0 s) "a";
  assert_string (String.Prototype.char_at 1 s) "日"
(* Indices 2,3 are the emoji surrogate pair, index 4 is "b" *)

let tests =
  [
    test "S15.5.4.4_A1: basic ASCII" basic_ascii;
    test "S15.5.4.4_A2: out of bounds" out_of_bounds;
    test "S15.5.4.4_A3: empty string" empty_string;
    test "S15.5.4.4_A4: Unicode BMP" unicode_bmp;
    test "S15.5.4.4_A5: Unicode surrogate pairs" unicode_surrogate_pairs;
    test "S15.5.4.4_A6: mixed content" mixed_content;
  ]

