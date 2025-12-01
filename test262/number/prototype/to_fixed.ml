(* TC39 Test262: Number.prototype.toFixed tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/Number/prototype/toFixed

   ECMA-262 Section: Number.prototype.toFixed(fractionDigits)

   Tests for Number.Prototype.to_fixed *)

module Number = Quickjs.Number

(* ===================================================================
   Basic toFixed functionality
   =================================================================== *)

let basic_fixed () =
  assert_string (Number.Prototype.to_fixed 2 3.14159) "3.14";
  assert_string (Number.Prototype.to_fixed 0 3.7) "4";
  assert_string (Number.Prototype.to_fixed 3 3.1) "3.100"

let rounding () =
  assert_string (Number.Prototype.to_fixed 1 1.25) "1.3";
  assert_string (Number.Prototype.to_fixed 0 0.5) "1"

let negative_numbers () =
  assert_string (Number.Prototype.to_fixed 2 (-3.14)) "-3.14"

let zero_fraction_digits () =
  assert_string (Number.Prototype.to_fixed 0 123.456) "123";
  assert_string (Number.Prototype.to_fixed 0 123.5) "124"

let high_precision () =
  assert_string (Number.Prototype.to_fixed 5 1.0) "1.00000";
  assert_string (Number.Prototype.to_fixed 10 0.5) "0.5000000000"

let integer_values () =
  assert_string (Number.Prototype.to_fixed 2 42.0) "42.00";
  assert_string (Number.Prototype.to_fixed 0 42.0) "42"

let small_values () =
  assert_string (Number.Prototype.to_fixed 2 0.001) "0.00";
  assert_string (Number.Prototype.to_fixed 4 0.001) "0.0010"

(* ===================================================================
   Special values (tc39/test262 edge cases)
   =================================================================== *)

let special_nan () =
  (* NaN.toFixed() returns "NaN" *)
  assert_string (Number.Prototype.to_fixed 2 Float.nan) "NaN"

let special_infinity () =
  (* Infinity.toFixed() returns "Infinity" *)
  assert_string (Number.Prototype.to_fixed 2 Float.infinity) "Infinity";
  assert_string (Number.Prototype.to_fixed 2 Float.neg_infinity) "-Infinity"

let special_zero () =
  assert_string (Number.Prototype.to_fixed 2 0.0) "0.00";
  assert_string (Number.Prototype.to_fixed 0 0.0) "0"

let tests =
  [
    test "S15.7.4.5_A1: basic fixed" basic_fixed;
    test "S15.7.4.5_A2: rounding" rounding;
    test "S15.7.4.5_A3: negative numbers" negative_numbers;
    test "S15.7.4.5_A4: zero fraction digits" zero_fraction_digits;
    test "S15.7.4.5_A5: high precision" high_precision;
    test "S15.7.4.5_A6: integer values" integer_values;
    test "S15.7.4.5_A7: small values" small_values;
    test "special: NaN" special_nan;
    test "special: Infinity" special_infinity;
    test "special: zero" special_zero;
  ]
