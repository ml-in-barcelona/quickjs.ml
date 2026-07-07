(* TC39 Test262: String.fromCharCode tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/fromCharCode

   ECMA-262 Section: String.fromCharCode(...codeUnits)

   Tests for String.from_char_code *)

module String = Quickjs.String

let no_arguments () = assert_string (String.from_char_code [||]) ""

let basic_ascii () =
  assert_string (String.from_char_code [| 65; 66; 67 |]) "ABC";
  assert_string (String.from_char_code [| 72; 101; 108; 108; 111 |]) "Hello"

let single_char () =
  assert_string (String.from_char_code [| 65 |]) "A";
  assert_string (String.from_char_code [| 0 |]) "\x00";
  assert_int (String.Prototype.length (String.from_char_code [| 0 |])) 1

let bmp_char () =
  (* U+3042 HIRAGANA LETTER A *)
  assert_string (String.from_char_code [| 0x3042 |]) "あ"

let to_uint16_wrapping () =
  (* S15.5.3.2_A2: every value is coerced with ToUint16 (modulo 2^16) *)
  assert_string (String.from_char_code [| 65536 + 65 |]) "A";
  assert_string (String.from_char_code [| (65536 * 2) + 66 |]) "B"

let to_uint16_negative () =
  (* ToUint16(-1) = 65535 = U+FFFF *)
  assert_int_opt
    (String.Prototype.char_code_at 0 (String.from_char_code [| -1 |]))
    (Some 0xFFFF);
  (* ToUint16(-65537) = 65535 *)
  assert_int_opt
    (String.Prototype.char_code_at 0 (String.from_char_code [| -65537 |]))
    (Some 0xFFFF)

let surrogate_pair_combines () =
  (* String.fromCharCode(0xD83D, 0xDE00) === "😀" *)
  let s = String.from_char_code [| 0xD83D; 0xDE00 |] in
  assert_string s "😀";
  assert_int (String.Prototype.length s) 2;
  assert_int_opt (String.Prototype.code_point_at 0 s) (Some 0x1F600)

let lone_surrogate_becomes_replacement () =
  (* JavaScript builds a lone-surrogate string; UTF-8 cannot represent it,
     so this module's policy substitutes U+FFFD (same policy as char_at) *)
  assert_string (String.from_char_code [| 0xD800 |]) "\u{FFFD}";
  assert_string (String.from_char_code [| 0xDC00 |]) "\u{FFFD}";
  (* high surrogate followed by a non-surrogate: JS gives "\uD800A" *)
  assert_string (String.from_char_code [| 0xD800; 0x41 |]) "\u{FFFD}A"

let mixed_content () =
  (* BMP text surrounding a surrogate pair *)
  assert_string (String.from_char_code [| 0x61; 0xD83D; 0xDE00; 0x62 |]) "a😀b"

let tests =
  [
    test "no arguments returns empty string" no_arguments;
    test "S15.5.3.2_A1: basic ASCII" basic_ascii;
    test "single char" single_char;
    test "BMP char" bmp_char;
    test "S15.5.3.2_A2: ToUint16 wrapping" to_uint16_wrapping;
    test "ToUint16 of negative values" to_uint16_negative;
    test "surrogate pair combines into code point" surrogate_pair_combines;
    test "lone surrogate becomes U+FFFD" lone_surrogate_becomes_replacement;
    test "mixed BMP and astral content" mixed_content;
  ]
