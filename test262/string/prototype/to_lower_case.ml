(* TC39 Test262: String.prototype.toLowerCase tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/toLowerCase

   ECMA-262 Section: String.prototype.toLowerCase()

   Tests for Unicode.lowercase and Unicode.lowercase_char *)

module Unicode = Quickjs.Unicode

(* ===================================================================
   String toLowerCase
   =================================================================== *)

let ascii_lowercase () =
  assert_string (Unicode.lowercase "HELLO") "hello";
  assert_string (Unicode.lowercase "Hello World") "hello world"

let already_lowercase () = assert_string (Unicode.lowercase "hello") "hello"
let unicode_lowercase () = assert_string (Unicode.lowercase "ÉCOLE") "école"
let empty_string () = assert_string (Unicode.lowercase "") ""

(* ===================================================================
   Character-level lowercase_char
   =================================================================== *)

let lowercase_char_ascii () =
  let result = Unicode.lowercase_char (Uchar.of_char 'A') in
  assert_int (List.length result) 1;
  assert_uchar (List.hd result) (Uchar.of_char 'a')

let lowercase_char_already_lower () =
  let result = Unicode.lowercase_char (Uchar.of_char 'a') in
  assert_int (List.length result) 1;
  assert_uchar (List.hd result) (Uchar.of_char 'a')

let tests =
  [
    test "S15.5.4.16_A1: ASCII lowercase" ascii_lowercase;
    test "S15.5.4.16_A2: already lowercase" already_lowercase;
    test "S15.5.4.16_A3: unicode lowercase" unicode_lowercase;
    test "S15.5.4.16_A4: empty string" empty_string;
    test "char: ASCII letter" lowercase_char_ascii;
    test "char: already lowercase" lowercase_char_already_lower;
  ]
