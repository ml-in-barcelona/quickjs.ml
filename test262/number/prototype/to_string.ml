(* TC39 Test262: Number.prototype.toString tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/Number/prototype/toString

   ECMA-262 Section: Number.prototype.toString([radix])

   Tests for Number.Prototype.to_string, Number.Prototype.to_radix, and Itoa functions *)

module Number = Quickjs.Number

(* ===================================================================
   Basic toString functionality (radix 10, default)
   =================================================================== *)

let basic_integers () =
  assert_string (Number.Prototype.to_string 0.0) "0";
  assert_string (Number.Prototype.to_string 1.0) "1";
  assert_string (Number.Prototype.to_string 42.0) "42";
  assert_string (Number.Prototype.to_string (-1.0)) "-1";
  assert_string (Number.Prototype.to_string (-42.0)) "-42"

let basic_decimals () =
  assert_string (Number.Prototype.to_string 3.14) "3.14";
  assert_string (Number.Prototype.to_string 0.5) "0.5";
  assert_string (Number.Prototype.to_string 0.125) "0.125";
  assert_string (Number.Prototype.to_string (-3.14)) "-3.14"

let special_values () =
  assert_string (Number.Prototype.to_string Float.nan) "NaN";
  assert_string (Number.Prototype.to_string Float.infinity) "Infinity";
  assert_string (Number.Prototype.to_string Float.neg_infinity) "-Infinity"

let negative_zero_default () =
  (* By default, -0 is displayed as "0" *)
  assert_string (Number.Prototype.to_string (-0.0)) "0"

let negative_zero_with_flag () =
  (* With show_minus_zero flag, -0 is displayed as "-0" *)
  let options = { Number.default_options with show_minus_zero = true } in
  assert_string (Number.Prototype.to_string ~options (-0.0)) "-0"

let large_numbers () =
  assert_string (Number.Prototype.to_string 1e10) "10000000000";
  assert_string (Number.Prototype.to_string 1e20) "100000000000000000000"

let very_large_exponential () =
  (* Very large numbers use exponential notation *)
  let s = Number.Prototype.to_string 1e21 in
  assert_bool (Stdlib.String.contains s 'e') true

let small_exponential () =
  (* Very small numbers use exponential notation *)
  let s = Number.Prototype.to_string 1e-10 in
  assert_bool (Stdlib.String.contains s 'e') true

(* ===================================================================
   toString with radix (Number.Prototype.to_radix)
   =================================================================== *)

let radix_binary () =
  assert_string (Number.Prototype.to_radix 2 8.0) "1000";
  assert_string (Number.Prototype.to_radix 2 255.0) "11111111"

let radix_octal () =
  assert_string (Number.Prototype.to_radix 8 64.0) "100";
  assert_string (Number.Prototype.to_radix 8 255.0) "377"

let radix_hexadecimal () =
  assert_string (Number.Prototype.to_radix 16 255.0) "ff";
  assert_string (Number.Prototype.to_radix 16 256.0) "100"

let radix_base36 () =
  assert_string (Number.Prototype.to_radix 36 35.0) "z";
  assert_string (Number.Prototype.to_radix 36 36.0) "10"

(* ===================================================================
   Integer toString (Itoa functions)
   =================================================================== *)

let itoa_int32 () =
  assert_string (Number.of_int32 0l) "0";
  assert_string (Number.of_int32 42l) "42";
  assert_string (Number.of_int32 (-42l)) "-42";
  assert_string (Number.of_int32 Int32.max_int) "2147483647";
  assert_string (Number.of_int32 Int32.min_int) "-2147483648"

let itoa_int64 () =
  assert_string (Number.of_int64 0L) "0";
  assert_string (Number.of_int64 42L) "42";
  assert_string (Number.of_int64 (-42L)) "-42";
  assert_string (Number.of_int64 Int64.max_int) "9223372036854775807";
  assert_string (Number.of_int64 Int64.min_int) "-9223372036854775808"

let itoa_int () =
  assert_string (Number.of_int 0) "0";
  assert_string (Number.of_int 42) "42";
  assert_string (Number.of_int (-42)) "-42"

let itoa_radix_binary () =
  assert_string (Number.of_int_radix ~radix:2 8) "1000";
  assert_string (Number.of_int_radix ~radix:2 255) "11111111"

let itoa_radix_hex () =
  assert_string (Number.of_int_radix ~radix:16 255) "ff";
  assert_string (Number.of_int_radix ~radix:16 256) "100"

let itoa_int64_radix () =
  assert_string (Number.of_int64_radix ~radix:16 255L) "ff";
  assert_string (Number.of_int64_radix ~radix:36 35L) "z"

(* ===================================================================
   Unsigned integer conversions
   =================================================================== *)

let itoa_uint32 () =
  assert_string (Number.of_uint32 (Unsigned.UInt32.of_int 0)) "0";
  assert_string (Number.of_uint32 (Unsigned.UInt32.of_int 42)) "42";
  assert_string (Number.of_uint32 Unsigned.UInt32.max_int) "4294967295"

let itoa_uint64 () =
  assert_string (Number.of_uint64 (Unsigned.UInt64.of_int 0)) "0";
  assert_string (Number.of_uint64 (Unsigned.UInt64.of_int 42)) "42";
  assert_string
    (Number.of_uint64 Unsigned.UInt64.max_int)
    "18446744073709551615"

let tests =
  [
    (* Basic toString (radix 10) *)
    test "S15.7.4.2_A1: basic integers" basic_integers;
    test "S15.7.4.2_A2: decimals" basic_decimals;
    test "S15.7.4.2_A3: special values" special_values;
    test "S15.7.4.2_A4: negative zero default" negative_zero_default;
    test "S15.7.4.2_A5: negative zero with flag" negative_zero_with_flag;
    test "S15.7.4.2_A6: large numbers" large_numbers;
    test "S15.7.4.2_A7: very large exponential" very_large_exponential;
    test "S15.7.4.2_A8: small exponential" small_exponential;
    (* toString with radix *)
    test "radix: binary" radix_binary;
    test "radix: octal" radix_octal;
    test "radix: hexadecimal" radix_hexadecimal;
    test "radix: base36" radix_base36;
    (* Itoa (integer specific) *)
    test "itoa: int32" itoa_int32;
    test "itoa: int64" itoa_int64;
    test "itoa: int" itoa_int;
    test "itoa: radix binary" itoa_radix_binary;
    test "itoa: radix hex" itoa_radix_hex;
    test "itoa: int64 radix" itoa_int64_radix;
    (* Unsigned variants *)
    test "itoa: uint32" itoa_uint32;
    test "itoa: uint64" itoa_uint64;
  ]
