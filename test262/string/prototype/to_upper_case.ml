(* TC39 Test262: String.prototype.toUpperCase tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/toUpperCase

   ECMA-262 Section: String.prototype.toUpperCase()

   Tests for Unicode.uppercase and Unicode.uppercase_char *)

module Unicode = Quickjs.Unicode

(* ===================================================================
   String toUpperCase
   =================================================================== *)

let ascii_uppercase () =
  assert_string (Unicode.uppercase "hello") "HELLO";
  assert_string (Unicode.uppercase "Hello World") "HELLO WORLD"

let already_uppercase () = assert_string (Unicode.uppercase "HELLO") "HELLO"
let unicode_uppercase () = assert_string (Unicode.uppercase "école") "ÉCOLE"

let german_sharp_s () =
  (* ß → SS - this is a case where one char becomes two *)
  assert_string (Unicode.uppercase "straße") "STRASSE"

let empty_string () = assert_string (Unicode.uppercase "") ""

(* ===================================================================
   Character-level uppercase_char
   =================================================================== *)

let uppercase_char_ascii () =
  let result = Unicode.uppercase_char (Uchar.of_char 'a') in
  assert_int (List.length result) 1;
  assert_uchar (List.hd result) (Uchar.of_char 'A')

let uppercase_char_sharp_s () =
  (* ß = U+00DF → SS (two characters) *)
  let result = Unicode.uppercase_char (Uchar.of_int 0x00DF) in
  assert_int (List.length result) 2;
  assert_uchar (List.nth result 0) (Uchar.of_char 'S');
  assert_uchar (List.nth result 1) (Uchar.of_char 'S')

let tests =
  [
    test "S15.5.4.18_A1: ASCII uppercase" ascii_uppercase;
    test "S15.5.4.18_A2: already uppercase" already_uppercase;
    test "S15.5.4.18_A3: unicode uppercase" unicode_uppercase;
    test "S15.5.4.18_A4: German sharp s" german_sharp_s;
    test "S15.5.4.18_A5: empty string" empty_string;
    test "char: ASCII letter" uppercase_char_ascii;
    test "char: sharp s expansion" uppercase_char_sharp_s;
  ]
