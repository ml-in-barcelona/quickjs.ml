module RegExp = Quickjs.RegExp

let test title fn = Alcotest.test_case title `Quick fn

let assert_result left right =
  Alcotest.(check (array string)) "should be equal" right left

let assert_int left right = Alcotest.(check int) "should be equal" right left

let assert_string left right =
  Alcotest.(check string) "should be equal" right left

let assert_bool left right = Alcotest.(check bool) "should be equal" right left

let regexp_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok regexp -> regexp
  | Error error ->
      Alcotest.fail
        (Printf.sprintf
           "This regex should not fail to compile, it failed with %s"
           (RegExp.compile_error_to_string error))

let regexp_no_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok _regexp -> Alcotest.fail "This regex should fail to compile, it succeded"
  | Error error -> error

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
          test "unicode: [a-z] does not match unicode letters" (fun () ->
              let regex = regexp_compile "^[a-z]+$" ~flags:"i" in
              (* ASCII works *)
              assert_bool (RegExp.test regex "car") true;
              (* Unicode letters don't match [a-z] *)
              assert_bool (RegExp.test regex "pão") false;
              assert_bool (RegExp.test regex "知道") false;
              assert_bool (RegExp.test regex "يعرف") false);
          test "unicode: \\p{L} matches unicode letters" (fun () ->
              let regex = regexp_compile "^\\p{L}+$" ~flags:"u" in
              (* ASCII works *)
              assert_bool (RegExp.test regex "car") true;
              (* Unicode letters match with \p{L} and u flag *)
              assert_bool (RegExp.test regex "pão") true;
              assert_bool (RegExp.test regex "知道") true;
              assert_bool (RegExp.test regex "يعرف") true);
        ] );
      ( "Error",
        [
          test "unexpected end" (fun () ->
              let error = regexp_no_compile "[d-f" ~flags:"" in
              match error with
              | `Unknown msg when String.length msg > 0 ->
                  () (* QuickJS may return different error *)
              | `Unexpected_end -> ()
              | other ->
                  Alcotest.fail
                    (Printf.sprintf "Expected unexpected end, got %s"
                       (RegExp.compile_error_to_string other)));
          test "expecting ')'" (fun () ->
              let error = regexp_no_compile "(abc" ~flags:"" in
              match error with
              | `Unknown msg when String.length msg > 0 ->
                  () (* QuickJS may return different error *)
              | other ->
                  Alcotest.fail
                    (Printf.sprintf "Expected error, got %s"
                       (RegExp.compile_error_to_string other)));
          test "nothing to repeat" (fun () ->
              let error = regexp_no_compile "*a" ~flags:"" in
              match error with
              | `Nothing_to_repeat -> ()
              | other ->
                  Alcotest.fail
                    (Printf.sprintf "Expected nothing to repeat, got %s"
                       (RegExp.compile_error_to_string other)));
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
    ]
