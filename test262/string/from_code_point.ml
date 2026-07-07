(* TC39 Test262: String.fromCodePoint tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/fromCodePoint

   ECMA-262 Section: String.fromCodePoint(...codePoints)

   Tests for String.from_code_point *)

module String = Quickjs.String

let no_arguments () = assert_string (String.from_code_point [||]) ""

let basic_ascii () =
  assert_string (String.from_code_point [| 65 |]) "A";
  assert_string (String.from_code_point [| 72; 101; 108; 108; 111 |]) "Hello"

let nul_char () =
  assert_string (String.from_code_point [| 0 |]) "\x00";
  assert_int (String.Prototype.length (String.from_code_point [| 0 |])) 1

let astral_code_point () =
  (* String.fromCodePoint(0x1F600) === "😀", a string of length 2 *)
  let s = String.from_code_point [| 0x1F600 |] in
  assert_string s "😀";
  assert_int (String.Prototype.length s) 2;
  assert_int_opt (String.Prototype.char_code_at 0 s) (Some 0xD83D);
  assert_int_opt (String.Prototype.char_code_at 1 s) (Some 0xDE00)

let max_code_point () =
  let s = String.from_code_point [| 0x10FFFF |] in
  assert_int (String.Prototype.length s) 2;
  assert_int_opt (String.Prototype.code_point_at 0 s) (Some 0x10FFFF)

let out_of_range_raises () =
  let expect_invalid f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail "Expected Invalid_argument"
  in
  (* JavaScript: RangeError: Invalid code point *)
  expect_invalid (fun () -> String.from_code_point [| 0x110000 |]);
  expect_invalid (fun () -> String.from_code_point [| -1 |]);
  expect_invalid (fun () -> String.from_code_point [| 65; -1 |]);
  expect_invalid (fun () -> String.from_code_point [| 65; 0x110000 |])

let surrogate_halves_pair_up () =
  (* String.fromCodePoint(0xD83D, 0xDE00) === String.fromCodePoint(0x1F600):
     code points are expanded to UTF-16 units, so adjacent halves pair up *)
  assert_string (String.from_code_point [| 0xD83D; 0xDE00 |]) "😀"

let lone_surrogate_becomes_replacement () =
  (* String.fromCodePoint(0xD800) is legal in JavaScript and builds a
     lone-surrogate string; UTF-8 cannot represent it, so this module's
     policy substitutes U+FFFD *)
  assert_string (String.from_code_point [| 0xD800 |]) "\u{FFFD}";
  assert_string (String.from_code_point [| 0xDFFF |]) "\u{FFFD}"

let mixed_content () =
  assert_string (String.from_code_point [| 0x61; 0x1F600; 0x62 |]) "a😀b";
  assert_int
    (String.Prototype.length (String.from_code_point [| 0x61; 0x1F600; 0x62 |]))
    4

let tests =
  [
    test "no arguments returns empty string" no_arguments;
    test "basic ASCII" basic_ascii;
    test "U+0000" nul_char;
    test "astral code point builds a surrogate pair" astral_code_point;
    test "maximum code point U+10FFFF" max_code_point;
    test "out of range raises (RangeError)" out_of_range_raises;
    test "adjacent surrogate halves pair up" surrogate_halves_pair_up;
    test "lone surrogate becomes U+FFFD" lone_surrogate_becomes_replacement;
    test "mixed BMP and astral content" mixed_content;
  ]
