(* TC39 Test262: parseInt tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/parseInt

   ECMA-262 Section: parseInt(string, radix)

   Tests for Global.parse_int *)

module Global = Quickjs.Global

(* ===================================================================
   Basic parseInt functionality - S15.1.2.2
   =================================================================== *)

let basic_integers () =
  assert_int_opt (Global.parse_int "0") (Some 0);
  assert_int_opt (Global.parse_int "42") (Some 42);
  assert_int_opt (Global.parse_int "-42") (Some (-42));
  assert_int_opt (Global.parse_int "123456789") (Some 123456789)

let leading_zeros () =
  (* Leading zeros should parse as decimal by default *)
  assert_int_opt (Global.parse_int "007") (Some 7);
  assert_int_opt (Global.parse_int "00123") (Some 123)

let with_sign () =
  assert_int_opt (Global.parse_int "+42") (Some 42);
  assert_int_opt (Global.parse_int "-42") (Some (-42));
  assert_int_opt (Global.parse_int "+0") (Some 0);
  assert_int_opt (Global.parse_int "-0") (Some 0)

(* ===================================================================
   Radix handling - S15.1.2.2_A2
   =================================================================== *)

let radix_binary () =
  assert_int_opt (Global.parse_int ~radix:2 "1010") (Some 10);
  assert_int_opt (Global.parse_int ~radix:2 "11111111") (Some 255);
  assert_int_opt (Global.parse_int ~radix:2 "-1010") (Some (-10))

let radix_octal () =
  assert_int_opt (Global.parse_int ~radix:8 "77") (Some 63);
  assert_int_opt (Global.parse_int ~radix:8 "755") (Some 493);
  assert_int_opt (Global.parse_int ~radix:8 "-10") (Some (-8))

let radix_hex () =
  assert_int_opt (Global.parse_int ~radix:16 "ff") (Some 255);
  assert_int_opt (Global.parse_int ~radix:16 "FF") (Some 255);
  (* Use a value that fits in 31 bits for 32-bit platform compatibility *)
  assert_int_opt (Global.parse_int ~radix:16 "7fffffff") (Some 0x7fffffff);
  assert_int_opt (Global.parse_int ~radix:16 "-ff") (Some (-255))

let radix_36 () =
  (* Radix 36 uses 0-9 and a-z *)
  assert_int_opt (Global.parse_int ~radix:36 "z") (Some 35);
  assert_int_opt (Global.parse_int ~radix:36 "10") (Some 36);
  assert_int_opt (Global.parse_int ~radix:36 "zz") (Some 1295)

let radix_auto_detect () =
  (* With radix 0, auto-detect based on prefix *)
  assert_int_opt (Global.parse_int ~radix:0 "0xff") (Some 255);
  assert_int_opt (Global.parse_int ~radix:0 "0xFF") (Some 255);
  assert_int_opt (Global.parse_int ~radix:0 "123") (Some 123)

let hex_prefix_with_radix_16 () =
  (* 0x prefix should work with explicit radix 16 *)
  assert_int_opt (Global.parse_int ~radix:16 "0xff") (Some 255);
  assert_int_opt (Global.parse_int ~radix:16 "0XFF") (Some 255)

(* ===================================================================
   Whitespace handling - S15.1.2.2_A1
   =================================================================== *)

let leading_whitespace () =
  (* parseInt should strip leading whitespace *)
  assert_int_opt (Global.parse_int "  42") (Some 42);
  assert_int_opt (Global.parse_int "\t42") (Some 42);
  assert_int_opt (Global.parse_int "\n42") (Some 42);
  assert_int_opt (Global.parse_int "   \t\n  42") (Some 42)

let trailing_non_numeric () =
  (* parseInt stops at first non-numeric character *)
  assert_int_opt (Global.parse_int "42abc") (Some 42);
  assert_int_opt (Global.parse_int "42.5") (Some 42);
  assert_int_opt (Global.parse_int "123 456") (Some 123)

(* ===================================================================
   Edge cases and invalid inputs
   =================================================================== *)

let invalid_strings () =
  assert_int_opt (Global.parse_int "") None;
  assert_int_opt (Global.parse_int "abc") None;
  assert_int_opt (Global.parse_int "   ") None

let invalid_radix () =
  (* Radix must be 0 or 2-36, invalid radix returns None *)
  assert_int_opt (Global.parse_int ~radix:1 "10") None;
  assert_int_opt (Global.parse_int ~radix:37 "10") None;
  assert_int_opt (Global.parse_int ~radix:(-1) "10") None

let invalid_digits_for_radix () =
  (* Digits invalid for the given radix *)
  assert_int_opt (Global.parse_int ~radix:2 "2") None;
  assert_int_opt (Global.parse_int ~radix:8 "9") None;
  assert_int_opt (Global.parse_int ~radix:10 "a") None

let partial_valid_digits () =
  (* Parse as many valid digits as possible *)
  assert_int_opt (Global.parse_int ~radix:2 "1012") (Some 5);
  (* "101" = 5, then "2" is invalid *)
  assert_int_opt (Global.parse_int ~radix:8 "789") (Some 7)
(* "7" = 7, then "8" is invalid *)

(* ===================================================================
   Large numbers
   =================================================================== *)

let large_numbers () =
  (* Test with max values that fit in OCaml's int type.
     On 64-bit: int is 63 bits, max = 4611686018427387903
     On 32-bit: int is 31 bits, max = 1073741823

     Note: We use int_of_string for large values because OCaml validates
     integer literals at compile time. Even inside a conditional, the
     compiler would reject literals that exceed the platform's int size. *)
  if Sys.int_size >= 63 then begin
    (* 64-bit platform: can test with JS MAX_SAFE_INTEGER *)
    let max_safe = int_of_string "9007199254740991" in
    assert_int_opt (Global.parse_int "9007199254740991") (Some max_safe);
    assert_int_opt (Global.parse_int "-9007199254740991") (Some (-max_safe))
  end
  else begin
    (* 32-bit platform: use max 31-bit signed int *)
    assert_int_opt (Global.parse_int "1073741823") (Some 1073741823);
    assert_int_opt (Global.parse_int "-1073741823") (Some (-1073741823))
  end

(* ===================================================================
   Test suite
   =================================================================== *)

let tests =
  [
    test "S15.1.2.2_A1: basic integers" basic_integers;
    test "S15.1.2.2_A1_T2: leading zeros" leading_zeros;
    test "S15.1.2.2_A1_T3: with sign" with_sign;
    test "S15.1.2.2_A2_T1: radix binary" radix_binary;
    test "S15.1.2.2_A2_T2: radix octal" radix_octal;
    test "S15.1.2.2_A2_T3: radix hex" radix_hex;
    test "S15.1.2.2_A2_T4: radix 36" radix_36;
    test "S15.1.2.2_A2_T5: radix auto-detect" radix_auto_detect;
    test "S15.1.2.2_A2_T6: hex prefix with radix 16" hex_prefix_with_radix_16;
    test "S15.1.2.2_A3_T1: leading whitespace" leading_whitespace;
    test "S15.1.2.2_A3_T2: trailing non-numeric" trailing_non_numeric;
    test "S15.1.2.2_A4_T1: invalid strings" invalid_strings;
    test "S15.1.2.2_A4_T2: invalid radix" invalid_radix;
    test "S15.1.2.2_A4_T3: invalid digits for radix" invalid_digits_for_radix;
    test "S15.1.2.2_A4_T4: partial valid digits" partial_valid_digits;
    test "S15.1.2.2_A5: large numbers" large_numbers;
  ]
