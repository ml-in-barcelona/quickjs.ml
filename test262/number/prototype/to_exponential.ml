(* TC39 Test262: Number.prototype.toExponential tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/Number/prototype/toExponential

   ECMA-262 Section: Number.prototype.toExponential(fractionDigits)

   Tests for Number.Prototype.to_exponential *)

module Number = Quickjs.Number

(* ===================================================================
   Basic toExponential functionality
   =================================================================== *)

let basic_exponential () =
  assert_string (Number.Prototype.to_exponential 2 123.456) "1.23e+2";
  assert_string (Number.Prototype.to_exponential 4 123.456) "1.2346e+2"

let small_numbers () =
  assert_string (Number.Prototype.to_exponential 2 0.00123) "1.23e-3"

let large_numbers () =
  assert_string (Number.Prototype.to_exponential 2 1234567.0) "1.23e+6"

let zero () = assert_string (Number.Prototype.to_exponential 2 0.0) "0.00e+0"
let one () = assert_string (Number.Prototype.to_exponential 2 1.0) "1.00e+0"

let negative () =
  assert_string (Number.Prototype.to_exponential 2 (-123.456)) "-1.23e+2"

(* ===================================================================
   Special values (tc39/test262 edge cases)
   =================================================================== *)

let special_nan () =
  (* NaN.toExponential() returns "NaN" *)
  assert_string (Number.Prototype.to_exponential 2 Float.nan) "NaN"

let special_infinity () =
  (* Infinity.toExponential() returns "Infinity" *)
  assert_string (Number.Prototype.to_exponential 2 Float.infinity) "Infinity";
  assert_string
    (Number.Prototype.to_exponential 2 Float.neg_infinity)
    "-Infinity"

let higher_precision () =
  assert_string (Number.Prototype.to_exponential 10 123.456) "1.2345600000e+2"

let tests =
  [
    test "S15.7.4.6_A1: basic exponential" basic_exponential;
    test "S15.7.4.6_A2: small numbers" small_numbers;
    test "S15.7.4.6_A3: large numbers" large_numbers;
    test "S15.7.4.6_A4: zero" zero;
    test "S15.7.4.6_A5: one" one;
    test "S15.7.4.6_A6: negative" negative;
    test "special: NaN" special_nan;
    test "special: Infinity" special_infinity;
    test "special: higher precision" higher_precision;
  ]
