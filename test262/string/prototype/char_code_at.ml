(* TC39 Test262: String.prototype.charCodeAt tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/charCodeAt

   ECMA-262 Section: String.prototype.charCodeAt(pos)

   Tests for String.Prototype.char_code_at *)

module String = Quickjs.String

(* ===================================================================
   Basic charCodeAt functionality - S15.5.4.5
   =================================================================== *)

let basic_ascii () =
  assert_int_opt (String.Prototype.char_code_at 0 "hello") (Some 104);
  (* 'h' *)
  assert_int_opt (String.Prototype.char_code_at 1 "hello") (Some 101);
  (* 'e' *)
  assert_int_opt (String.Prototype.char_code_at 0 "A") (Some 65);
  assert_int_opt (String.Prototype.char_code_at 0 "0") (Some 48)

let out_of_bounds () =
  (* Out of bounds returns None (NaN in JavaScript) *)
  assert_int_opt (String.Prototype.char_code_at (-1) "hello") None;
  assert_int_opt (String.Prototype.char_code_at 5 "hello") None;
  assert_int_opt (String.Prototype.char_code_at 100 "hello") None

let empty_string () =
  assert_int_opt (String.Prototype.char_code_at 0 "") None

let unicode_bmp () =
  (* BMP characters - returns their Unicode code point *)
  assert_int_opt (String.Prototype.char_code_at 3 "café") (Some 0xe9);
  (* é = U+00E9 *)
  assert_int_opt (String.Prototype.char_code_at 0 "日") (Some 0x65e5)
(* 日 = U+65E5 *)

let unicode_surrogate_pairs () =
  (* Characters outside BMP return surrogate code units *)
  (* "😀" is U+1F600 = surrogate pair D83D DE00 *)
  let emoji = "😀" in
  assert_int_opt (String.Prototype.char_code_at 0 emoji) (Some 0xD83D);
  (* high surrogate *)
  assert_int_opt (String.Prototype.char_code_at 1 emoji) (Some 0xDE00)
(* low surrogate *)

let special_characters () =
  assert_int_opt (String.Prototype.char_code_at 0 "\n") (Some 10);
  assert_int_opt (String.Prototype.char_code_at 0 "\t") (Some 9);
  assert_int_opt (String.Prototype.char_code_at 0 " ") (Some 32)

let tests =
  [
    test "S15.5.4.5_A1: basic ASCII" basic_ascii;
    test "S15.5.4.5_A2: out of bounds" out_of_bounds;
    test "S15.5.4.5_A3: empty string" empty_string;
    test "S15.5.4.5_A4: Unicode BMP" unicode_bmp;
    test "S15.5.4.5_A5: Unicode surrogate pairs" unicode_surrogate_pairs;
    test "S15.5.4.5_A6: special characters" special_characters;
  ]

