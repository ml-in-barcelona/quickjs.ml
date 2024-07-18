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
  | Error (_, error) ->
      Alcotest.fail
        (Printf.sprintf
           "This regex should not fail to compile, it failed with %s" error)

let regexp_no_compile re ~flags =
  match RegExp.compile re ~flags with
  | Ok _regexp -> Alcotest.fail "This regex should fail to compile, it succeded"
  | Error (_, error) -> error

let () =
  Alcotest.run "RegExp"
    [
      ( "Success",
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
              (* TODO: Support named groups in melange.js and make them available within the result *)
              let regex =
                regexp_compile "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})"
                  ~flags:""
              in
              let input = "Today's date is 2024-07-17" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result)
                [| "2024-07-17"; "2024"; "07"; "17" |]);
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
    ]
