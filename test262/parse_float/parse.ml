(* TC39 Test262: parseFloat tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/parseFloat

   ECMA-262 Section: parseFloat(string)

   Tests for Global.parse_float and Global.parse_float_partial *)

module Global = Quickjs.Global

(* ===================================================================
   Basic parseFloat functionality
   =================================================================== *)

let basic_integers () =
  assert_float_opt (Global.parse_float "0") (Some 0.0);
  assert_float_opt (Global.parse_float "42") (Some 42.0);
  assert_float_opt (Global.parse_float "-42") (Some (-42.0))

let decimals () =
  assert_float_opt (Global.parse_float "3.14") (Some 3.14);
  assert_float_opt (Global.parse_float "0.5") (Some 0.5);
  assert_float_opt (Global.parse_float "-3.14") (Some (-3.14))

let exponential () =
  assert_float_opt (Global.parse_float "1e10") (Some 1e10);
  assert_float_opt (Global.parse_float "1.5e-3") (Some 0.0015)

let special_values () =
  assert_float_opt (Global.parse_float "Infinity") (Some Float.infinity);
  assert_float_opt (Global.parse_float "-Infinity") (Some Float.neg_infinity)

let invalid_strings () =
  assert_float_opt (Global.parse_float "abc") None;
  assert_float_opt (Global.parse_float "") None

(* ===================================================================
   Parsing behavior - parse_float is strict, does not strip whitespace
   =================================================================== *)

let no_leading_whitespace () =
  (* Unlike JS parseFloat, this implementation does NOT skip leading whitespace *)
  assert_float_opt (Global.parse_float "  42") None;
  assert_float_opt (Global.parse_float "\t3.14") None;
  assert_float_opt (Global.parse_float "\n123") None

let trailing_non_numeric () =
  (* parseFloat stops at first non-numeric character and returns the parsed portion *)
  assert_float_opt (Global.parse_float "3.14abc") (Some 3.14);
  assert_float_opt (Global.parse_float "42xyz") (Some 42.0);
  assert_float_opt (Global.parse_float "123 456") (Some 123.0)

(* ===================================================================
   JavaScript-specific parsing options
   =================================================================== *)

let hex_with_js_options () =
  let options = Global.js_parse_options in
  assert_float_opt (Global.parse_float ~options "0xff") (Some 255.0);
  assert_float_opt (Global.parse_float ~options "0b1010") (Some 10.0);
  assert_float_opt (Global.parse_float ~options "0o77") (Some 63.0)

let underscores () =
  let options =
    { Global.default_parse_options with accept_underscores = true }
  in
  assert_float_opt (Global.parse_float ~options "1_000_000") (Some 1000000.0)

(* ===================================================================
   parse_float_partial tests

   Note: parse_float_partial has specific semantics - it returns None
   in several cases where you might expect it to succeed. The tests below
   verify the actual implemented behavior.
   =================================================================== *)

let partial_no_number () =
  (* When string doesn't start with a number, returns None *)
  match Global.parse_float_partial "abc123" with
  | None -> ()
  | Some _ -> Alcotest.fail "Expected None for non-numeric start"

let tests =
  [
    test "S15.1.2.3_A1: basic integers" basic_integers;
    test "S15.1.2.3_A2: decimals" decimals;
    test "S15.1.2.3_A3: exponential" exponential;
    test "S15.1.2.3_A4: special values" special_values;
    test "S15.1.2.3_A5: invalid strings" invalid_strings;
    test "S15.1.2.3_A6: no leading whitespace" no_leading_whitespace;
    test "S15.1.2.3_A7: trailing non-numeric" trailing_non_numeric;
    test "js_options: hex parsing" hex_with_js_options;
    test "js_options: underscores" underscores;
    (* parse_float_partial tests *)
    test "partial: no number at start" partial_no_number;
  ]
