(* TC39 Test262: Number.prototype.toPrecision tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/Number/prototype/toPrecision

   ECMA-262 Section: Number.prototype.toPrecision(precision)

   Tests for Number.Prototype.to_precision *)

module Number = Quickjs.Number

(* ===================================================================
   Basic toPrecision functionality
   =================================================================== *)

let basic_precision () =
  assert_string (Number.Prototype.to_precision 4 123.456) "123.5";
  assert_string (Number.Prototype.to_precision 2 123.456) "1.2e+2";
  assert_string (Number.Prototype.to_precision 6 123.456) "123.456"

let small_numbers () =
  assert_string (Number.Prototype.to_precision 2 0.000123) "0.00012"

let exact_precision () =
  assert_string (Number.Prototype.to_precision 1 5.0) "5";
  assert_string (Number.Prototype.to_precision 3 5.0) "5.00"

let large_numbers () =
  assert_string (Number.Prototype.to_precision 3 1234.0) "1.23e+3";
  assert_string (Number.Prototype.to_precision 5 1234.0) "1234.0"

(* ===================================================================
   Special values (tc39/test262 edge cases)
   =================================================================== *)

let special_nan () =
  (* NaN.toPrecision() returns "NaN" *)
  assert_string (Number.Prototype.to_precision 2 Float.nan) "NaN"

let special_infinity () =
  (* Infinity.toPrecision() returns "Infinity" *)
  assert_string (Number.Prototype.to_precision 2 Float.infinity) "Infinity";
  assert_string (Number.Prototype.to_precision 2 Float.neg_infinity) "-Infinity"

let special_zero () =
  assert_string (Number.Prototype.to_precision 1 0.0) "0";
  assert_string (Number.Prototype.to_precision 3 0.0) "0.00"

let negative_numbers () =
  assert_string (Number.Prototype.to_precision 3 (-123.456)) "-123";
  assert_string (Number.Prototype.to_precision 2 (-5.0)) "-5.0"

let tests =
  [
    test "S15.7.4.7_A1: basic precision" basic_precision;
    test "S15.7.4.7_A2: small numbers" small_numbers;
    test "S15.7.4.7_A3: exact precision" exact_precision;
    test "S15.7.4.7_A4: large numbers" large_numbers;
    test "special: NaN" special_nan;
    test "special: Infinity" special_infinity;
    test "special: zero" special_zero;
    test "special: negative" negative_numbers;
  ]
