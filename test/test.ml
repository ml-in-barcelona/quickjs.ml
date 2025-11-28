module RegExp = Quickjs.RegExp
module Number = Quickjs.Number

let test title fn = Alcotest.test_case title `Quick fn

let assert_result left right =
  Alcotest.(check (array string)) "should be equal" right left

let assert_int left right = Alcotest.(check int) "should be equal" right left

let assert_string left right =
  Alcotest.(check string) "should be equal" right left

let assert_bool left right = Alcotest.(check bool) "should be equal" right left

let assert_float left right =
  Alcotest.(check (float 0.0001)) "should be equal" right left

let assert_nan value =
  Alcotest.(check bool) "should be NaN" true (Float.is_nan value)

let regexp_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok regexp -> regexp
  | Error (_, error) ->
      Alcotest.fail
        (Printf.sprintf
           "This regex should not fail to compile, it failed with %s" error)

let regexp_no_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok _regexp -> Alcotest.fail "This regex should fail to compile, it succeded"
  | Error (_, error) -> error

let () =
  Alcotest.run "quickjs"
    [
      ( "RegExp",
        [
          test "flags" (fun () ->
              let regex = regexp_compile "\\d" ~flags:"" in
              assert_string (RegExp.flags regex) "";
              let regex = regexp_compile "\\d" ~flags:"g" in
              assert_string (RegExp.flags regex) "g";
              let regex = regexp_compile "\\d" ~flags:"gy" in
              assert_string (RegExp.flags regex) "gy");
          test "test" (fun () ->
              let regex = regexp_compile "[0-9]+" ~flags:"" in
              let result = RegExp.test regex "abc123xyz" in
              assert_bool result true;
              let regex = regexp_compile "[0-9]+" ~flags:"" in
              let result = RegExp.test regex "abc" in
              assert_bool result false);
          test "basic" (fun () ->
              let regex = regexp_compile "[0-9]+" ~flags:"" in
              let result = RegExp.exec regex "abc123xyz" in
              assert_result (RegExp.captures result) [| "123" |]);
          test "exec" (fun () ->
              let regex = regexp_compile "[0-9]+" ~flags:"" in
              let result = RegExp.exec regex "abc00123xyz456_0" in
              assert_result (RegExp.captures result) [| "00123" |];
              let result = RegExp.exec regex "abc00123xyz456_0" in
              assert_result (RegExp.captures result) [| "00123" |]);
          test "basic text" (fun () ->
              let regex = regexp_compile "a" ~flags:"" in
              let result = RegExp.exec regex "bbb" in
              assert_result (RegExp.captures result) [||];
              let result = RegExp.exec regex "bbba" in
              assert_result (RegExp.captures result) [| "a" |]);
          test "with i (ignorecase)" (fun () ->
              let regex = regexp_compile "a" ~flags:"i" in
              let result = RegExp.exec regex "123bA" in
              assert_result (RegExp.captures result) [| "A" |];
              let result = RegExp.exec regex "123ba" in
              assert_result (RegExp.captures result) [| "a" |]);
          test "with m (multiline)" (fun () ->
              let regex = regexp_compile "^d" ~flags:"m" in
              let result = RegExp.exec regex "123bA" in
              assert_result (RegExp.captures result) [||];
              let result = RegExp.exec regex "123bA\n123" in
              assert_result (RegExp.captures result) [||];
              let result = RegExp.exec regex "david" in
              assert_result (RegExp.captures result) [| "d" |];
              let result = RegExp.exec regex "123bA\ndavid" in
              assert_result (RegExp.captures result) [| "d" |]);
          test "with g (global)" (fun () ->
              let regex = regexp_compile "[0-9]+" ~flags:"g" in
              let input = "abc00123xyz456_0" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "00123" |];
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "456" |];
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "0" |]);
          test "with y (sticky)" (fun () ->
              let regex = regexp_compile "foo" ~flags:"y" in
              assert_int (RegExp.lastIndex regex) 0;
              let input = "foofoofoo" in
              let result = RegExp.exec regex input in
              assert_int (RegExp.lastIndex regex) 3;
              assert_result (RegExp.captures result) [| "foo" |];
              let result = RegExp.exec regex input in
              assert_int (RegExp.lastIndex regex) 6;
              assert_result (RegExp.captures result) [| "foo" |];
              let result = RegExp.exec regex input in
              assert_int (RegExp.lastIndex regex) 9;
              assert_result (RegExp.captures result) [| "foo" |];
              let result = RegExp.exec regex input in
              assert_int (RegExp.lastIndex regex) 0;
              assert_result (RegExp.captures result) [||]);
          test "sticky vs global" (fun () ->
              let input = "abc xyz abc" in
              let sticky = regexp_compile "abc" ~flags:"y" in
              let global = regexp_compile "abc" ~flags:"g" in
              assert_bool (RegExp.test global input) true;
              assert_bool (RegExp.test global input) true;
              assert_bool (RegExp.test sticky input) true;
              assert_bool (RegExp.test sticky input) false);
          test "groups" (fun () ->
              let regex = regexp_compile "(xyz)" ~flags:"" in
              let input = "xyz yz xyzx xzy" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "xyz"; "xyz" |]);
          test "named groups" (fun () ->
              let regex =
                regexp_compile "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})"
                  ~flags:""
              in
              let input = "Today's date is 2024-07-17" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result)
                [| "2024-07-17"; "2024"; "07"; "17" |];
              (* Test accessing groups by name *)
              assert_string (Option.get (RegExp.group "year" result)) "2024";
              assert_string (Option.get (RegExp.group "month" result)) "07";
              assert_string (Option.get (RegExp.group "day" result)) "17";
              (* Test that non-existent group returns None *)
              assert_bool (Option.is_none (RegExp.group "hour" result)) true;
              (* Test groups function returns all named groups *)
              let groups = RegExp.groups result in
              assert_int (List.length groups) 3);
          test "named groups with global flag" (fun () ->
              let regex = regexp_compile "(?<word>\\w+)" ~flags:"g" in
              let input = "hello world" in
              let result = RegExp.exec regex input in
              assert_string (Option.get (RegExp.group "word" result)) "hello";
              let result = RegExp.exec regex input in
              assert_string (Option.get (RegExp.group "word" result)) "world");
          test "mixed named and unnamed groups" (fun () ->
              let regex =
                regexp_compile "(\\d+)-(?<name>\\w+)-(\\d+)" ~flags:""
              in
              let input = "123-test-456" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result)
                [| "123-test-456"; "123"; "test"; "456" |];
              (* Only the named group should be in groups *)
              assert_string (Option.get (RegExp.group "name" result)) "test";
              let groups = RegExp.groups result in
              assert_int (List.length groups) 1);
          test "no named groups returns empty list" (fun () ->
              let regex = regexp_compile "(\\d+)" ~flags:"" in
              let input = "123" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "123"; "123" |];
              let groups = RegExp.groups result in
              assert_int (List.length groups) 0);
          test "no match returns empty groups" (fun () ->
              let regex = regexp_compile "(?<num>\\d+)" ~flags:"" in
              let input = "no numbers here" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [||];
              let groups = RegExp.groups result in
              assert_int (List.length groups) 0;
              assert_bool (Option.is_none (RegExp.group "num" result)) true);
          test "index" (fun () ->
              let regex = regexp_compile "World" ~flags:"" in
              let input = "Hello World" in
              let result = RegExp.exec regex input in
              assert_int (RegExp.index result) 6);
          test "lastIndex and index" (fun () ->
              let regex = regexp_compile "hello" ~flags:"g" in
              let input = "hello world hello" in
              let result = RegExp.exec regex input in
              assert_int (RegExp.index result) 0;
              assert_int (RegExp.lastIndex regex) 5;
              let result = RegExp.exec regex input in
              assert_int (RegExp.index result) 12;
              assert_int (RegExp.lastIndex regex) 17;
              let result = RegExp.exec regex input in
              assert_int (RegExp.index result) 0;
              assert_int (RegExp.lastIndex regex) 0);
          (* https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/lookBehind/word-boundary.js *)
          test "groups with (?: )" (fun () ->
              let regex = regexp_compile "(?<=\\b)[d-f]{3}" ~flags:"" in
              let input = "def" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "def" |]);
          (* https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/lookBehind/negative.js#L21C22-L21C28 *)
          test "negative" (fun () ->
              let regex = regexp_compile "(?<!abc)\\w\\w\\w" ~flags:"" in
              let input = "abcdef" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "abc" |]);
          test "http/s" (fun () ->
              let pattern = "^[https?]+:\\/\\/((w{3}\\.)?[\\w+]+)\\.[\\w+]+$" in
              let regex = regexp_compile pattern ~flags:"" in
              assert_bool (RegExp.test regex "https://www.example.com") true;
              assert_bool (RegExp.test regex "http://example.com") true;
              assert_bool (RegExp.test regex "https://example") false);
        ] );
      ( "Error",
        [
          test "unexpected end" (fun () ->
              let error = regexp_no_compile "[d-f" ~flags:"" in
              assert_string error "unexpected end");
          test "expecting ')'" (fun () ->
              let error = regexp_no_compile "(abc" ~flags:"" in
              assert_string error "expecting ')'");
          test "nothing to repeat" (fun () ->
              let error = regexp_no_compile "*a" ~flags:"" in
              assert_string error "nothing to repeat");
        ] );
      ( "Error in some engines, but not in JavaScript (neither in QuickJS)",
        [
          test "a{,}" (fun () ->
              let regex = regexp_compile "a{,}" ~flags:"" in
              assert_string (RegExp.source regex) "a{,}");
          test "\\1(a)" (fun () ->
              let regex = regexp_compile "\\1(a)" ~flags:"" in
              assert_string (RegExp.source regex) "\\1(a)");
          test "[a-z[]" (fun () ->
              let regex = regexp_compile "[a-z[]" ~flags:"" in
              assert_string (RegExp.source regex) "[a-z[]");
        ] );
      ( "Number.parseInt",
        [
          test "basic decimal" (fun () ->
              assert_float (Number.parseInt "123") 123.0;
              assert_float (Number.parseInt "  123") 123.0;
              assert_float (Number.parseInt "  123r") 123.0;
              assert_float (Number.parseInt "0") 0.0);
          test "with sign" (fun () ->
              assert_float (Number.parseInt "-123") (-123.0);
              assert_float (Number.parseInt "+123") 123.0;
              assert_float (Number.parseInt "  -123") (-123.0));
          test "hexadecimal" (fun () ->
              assert_float (Number.parseInt "0x123") 291.0;
              assert_float (Number.parseInt "0X123") 291.0;
              assert_float (Number.parseInt "0xff") 255.0;
              assert_float (Number.parseInt "0xFF") 255.0);
          test "with radix" (fun () ->
              assert_float (Number.parseInt ~radix:16 "ff") 255.0;
              assert_float (Number.parseInt ~radix:2 "1010") 10.0;
              assert_float (Number.parseInt ~radix:8 "77") 63.0;
              assert_float (Number.parseInt ~radix:36 "z") 35.0);
          test "radix 16 with 0x prefix" (fun () ->
              assert_float (Number.parseInt ~radix:16 "0xff") 255.0);
          test "invalid radix returns NaN" (fun () ->
              assert_nan (Number.parseInt ~radix:1 "123");
              assert_nan (Number.parseInt ~radix:37 "123"));
          test "no valid digits returns NaN" (fun () ->
              assert_nan (Number.parseInt "abc");
              assert_nan (Number.parseInt "");
              assert_nan (Number.parseInt "   "));
          test "octal prefix not supported by default" (fun () ->
              (* JavaScript's parseInt does not auto-detect octal with 0o prefix *)
              assert_float (Number.parseInt "0o123") 0.0);
          test "stops at invalid character" (fun () ->
              assert_float (Number.parseInt "123abc") 123.0;
              assert_float (Number.parseInt "12.34") 12.0);
        ] );
      ( "Number.parseFloat",
        [
          test "basic decimal" (fun () ->
              assert_float (Number.parseFloat "123") 123.0;
              assert_float (Number.parseFloat "  123") 123.0;
              assert_float (Number.parseFloat "123.456") 123.456;
              assert_float (Number.parseFloat "0.5") 0.5);
          test "with sign" (fun () ->
              assert_float (Number.parseFloat "-123.456") (-123.456);
              assert_float (Number.parseFloat "+123.456") 123.456;
              assert_float (Number.parseFloat "-.5") (-0.5));
          test "scientific notation" (fun () ->
              assert_float (Number.parseFloat "1e10") 1e10;
              assert_float (Number.parseFloat "1E10") 1e10;
              assert_float (Number.parseFloat "1.5e-3") 0.0015;
              assert_float (Number.parseFloat "123.2e3") 123200.0);
          test "Infinity" (fun () ->
              assert_float (Number.parseFloat "Infinity") infinity;
              assert_float (Number.parseFloat "-Infinity") neg_infinity;
              assert_float (Number.parseFloat "  Infinity") infinity);
          test "no valid number returns NaN" (fun () ->
              assert_nan (Number.parseFloat "abc");
              assert_nan (Number.parseFloat "");
              assert_nan (Number.parseFloat "   "));
          test "hexadecimal returns 0" (fun () ->
              (* JavaScript parseFloat does not parse hex *)
              assert_float (Number.parseFloat "0x1234") 0.0);
          test "stops at invalid character" (fun () ->
              assert_float (Number.parseFloat "123abc") 123.0;
              assert_float (Number.parseFloat "123.456xyz") 123.456);
          test "leading dot" (fun () ->
              assert_float (Number.parseFloat ".5") 0.5;
              assert_float (Number.parseFloat ".123e2") 12.3);
        ] );
      ( "Number.isNaN",
        [
          test "returns true for NaN" (fun () ->
              assert_bool (Number.isNaN nan) true;
              assert_bool (Number.isNaN (Number.parseInt "abc")) true);
          test "returns false for numbers" (fun () ->
              assert_bool (Number.isNaN 0.0) false;
              assert_bool (Number.isNaN infinity) false;
              assert_bool (Number.isNaN neg_infinity) false;
              assert_bool (Number.isNaN 123.456) false);
        ] );
      ( "Number.isFinite",
        [
          test "returns true for finite numbers" (fun () ->
              assert_bool (Number.isFinite 0.0) true;
              assert_bool (Number.isFinite 123.456) true;
              assert_bool (Number.isFinite (-123.456)) true);
          test "returns false for non-finite values" (fun () ->
              assert_bool (Number.isFinite nan) false;
              assert_bool (Number.isFinite infinity) false;
              assert_bool (Number.isFinite neg_infinity) false);
        ] );
      ( "Number.isInteger",
        [
          test "returns true for integers" (fun () ->
              assert_bool (Number.isInteger 0.0) true;
              assert_bool (Number.isInteger 123.0) true;
              assert_bool (Number.isInteger (-123.0)) true);
          test "returns false for non-integers" (fun () ->
              assert_bool (Number.isInteger 123.456) false;
              assert_bool (Number.isInteger nan) false;
              assert_bool (Number.isInteger infinity) false);
        ] );
    ]
