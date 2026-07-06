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

let incomplete_exponents () =
  assert_float_opt (Global.parse_float "1e") (Some 1.0);
  assert_float_opt (Global.parse_float "1e+") (Some 1.0);
  assert_float_opt (Global.parse_float "1e-") (Some 1.0);
  assert_float_opt (Global.parse_float "1E") (Some 1.0);
  assert_float_opt (Global.parse_float "1E+") (Some 1.0);
  assert_float_opt (Global.parse_float "1E-") (Some 1.0);
  assert_float_opt (Global.parse_float "42e") (Some 42.0);
  assert_float_opt (Global.parse_float "3.14e") (Some 3.14);
  assert_float_opt (Global.parse_float "3.14e+") (Some 3.14);
  assert_float_opt (Global.parse_float "3.14e-") (Some 3.14)

let special_values () =
  assert_float_opt (Global.parse_float "Infinity") (Some Float.infinity);
  assert_float_opt (Global.parse_float "-Infinity") (Some Float.neg_infinity);
  assert_float_opt (Global.parse_float "+Infinity") (Some Float.infinity)

let invalid_strings () =
  assert_float_opt (Global.parse_float "abc") None;
  assert_float_opt (Global.parse_float "") None;
  (* "NaN" is not part of parseFloat's grammar; JavaScript returns NaN for
     it just like for any other unparsable input, which maps to None here.
     In particular strings starting with 'n'/'N' are not special. *)
  assert_float_opt (Global.parse_float "NaN") None;
  assert_float_opt (Global.parse_float "never") None;
  assert_float_opt (Global.parse_float "nope") None

(* ===================================================================
   Whitespace handling - S15.1.2.3_A1
   parseFloat skips leading whitespace (StrWhiteSpace)
   =================================================================== *)

let leading_whitespace () =
  assert_float_opt (Global.parse_float "  42") (Some 42.0);
  assert_float_opt (Global.parse_float "\t3.14") (Some 3.14);
  assert_float_opt (Global.parse_float "\n123") (Some 123.0);
  (* Unicode whitespace: NBSP and LINE SEPARATOR *)
  assert_float_opt (Global.parse_float "\xc2\xa042") (Some 42.0);
  assert_float_opt (Global.parse_float "\xe2\x80\xa842") (Some 42.0)

let whitespace_only () =
  assert_float_opt (Global.parse_float "   ") None;
  assert_float_opt (Global.parse_float "\t\n") None

let trailing_non_numeric () =
  (* parseFloat stops at first non-numeric character and returns the parsed portion *)
  assert_float_opt (Global.parse_float "3.14abc") (Some 3.14);
  assert_float_opt (Global.parse_float "42xyz") (Some 42.0);
  assert_float_opt (Global.parse_float "123 456") (Some 123.0)

(* ===================================================================
   JavaScript-specific parsing options
   =================================================================== *)

let hex_with_number_options () =
  let options = Global.js_number_options in
  assert_float_opt (Global.parse_float ~options "0xff") (Some 255.0);
  assert_float_opt (Global.parse_float ~options "0b1010") (Some 10.0);
  assert_float_opt (Global.parse_float ~options "0o77") (Some 63.0)

let hex_without_options () =
  (* JavaScript's parseFloat never interprets radix prefixes *)
  assert_float_opt (Global.parse_float "0xff") (Some 0.0)

let underscores () =
  let options =
    { Global.default_parse_options with accept_underscores = true }
  in
  assert_float_opt (Global.parse_float ~options "1_000_000") (Some 1000000.0)

(* ===================================================================
   parse_float_partial tests
   =================================================================== *)

let partial_basic () =
  (match Global.parse_float_partial "3.14abc" with
  | Some (value, rest) ->
      assert_float value 3.14;
      assert_string rest "abc"
  | None -> Alcotest.fail "Expected Some (3.14, \"abc\")");
  match Global.parse_float_partial "42" with
  | Some (value, rest) ->
      assert_float value 42.0;
      assert_string rest ""
  | None -> Alcotest.fail "Expected Some (42.0, \"\")"

let partial_whitespace () =
  match Global.parse_float_partial "  1.5 rest" with
  | Some (value, rest) ->
      assert_float value 1.5;
      assert_string rest " rest"
  | None -> Alcotest.fail "Expected Some (1.5, \" rest\")"

let partial_no_number () =
  (* When string doesn't start with a number, returns None *)
  (match Global.parse_float_partial "abc123" with
  | None -> ()
  | Some _ -> Alcotest.fail "Expected None for non-numeric start");
  match Global.parse_float_partial "" with
  | None -> ()
  | Some _ -> Alcotest.fail "Expected None for empty string"

let tests =
  [
    test "S15.1.2.3_A1: basic integers" basic_integers;
    test "S15.1.2.3_A2: decimals" decimals;
    test "S15.1.2.3_A3: exponential" exponential;
    test "S15.1.2.3_A4: special values" special_values;
    test "S15.1.2.3_A5: invalid strings" invalid_strings;
    test "S15.1.2.3_A6: leading whitespace is skipped" leading_whitespace;
    test "S15.1.2.3_A6_T2: whitespace only" whitespace_only;
    test "S15.1.2.3_A7: trailing non-numeric" trailing_non_numeric;
    test "S15.1.2.3_A8: incomplete exponents" incomplete_exponents;
    test "js_number_options: hex parsing" hex_with_number_options;
    test "parseFloat: no radix prefixes by default" hex_without_options;
    test "js_number_options: underscores" underscores;
    (* parse_float_partial tests *)
    test "partial: basic" partial_basic;
    test "partial: skips whitespace" partial_whitespace;
    test "partial: no number at start" partial_no_number;
  ]
