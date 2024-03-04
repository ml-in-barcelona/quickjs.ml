open Quickjs

let test title fn = Alcotest.test_case title `Quick fn

let assert_result left right =
  Alcotest.(check (array string)) "should be equal" right left

let assert_int left right = Alcotest.(check int) "should be equal" right left

let assert_string left right =
  Alcotest.(check string) "should be equal" right left

let assert_bool left right = Alcotest.(check bool) "should be equal" right left

let () =
  Alcotest.run "RegExp"
    [
      ( "test",
        [
          test "flags" (fun () ->
              let regex = RegExp.compile "\\d" "" in
              assert_string (RegExp.flags regex) "";
              let regex = RegExp.compile "\\d" "g" in
              assert_string (RegExp.flags regex) "g";
              let regex = RegExp.compile "\\d" "gy" in
              assert_string (RegExp.flags regex) "gy");
          test "test" (fun () ->
              let regex = RegExp.compile "[0-9]+" "" in
              let result = RegExp.test regex "abc123xyz" in
              assert_bool result true;
              let regex = RegExp.compile "[0-9]+" "" in
              let result = RegExp.test regex "abc" in
              assert_bool result false);
          test "basic" (fun () ->
              let regex = RegExp.compile "[0-9]+" "" in
              let result = RegExp.exec regex "abc123xyz" in
              assert_result (RegExp.captures result) [| "123" |]);
          test "exec" (fun () ->
              let regex = RegExp.compile "[0-9]+" "" in
              let result = RegExp.exec regex "abc00123xyz456_0" in
              assert_result (RegExp.captures result) [| "00123" |];
              let result = RegExp.exec regex "abc00123xyz456_0" in
              assert_result (RegExp.captures result) [| "00123" |]);
          test "basic text" (fun () ->
              let regex = RegExp.compile "a" "" in
              let result = RegExp.exec regex "bbb" in
              assert_result (RegExp.captures result) [||];
              let result = RegExp.exec regex "bbba" in
              assert_result (RegExp.captures result) [| "a" |]);
          test "with i (ignorecase)" (fun () ->
              let regex = RegExp.compile "a" "i" in
              let result = RegExp.exec regex "123bA" in
              assert_result (RegExp.captures result) [| "A" |];
              let result = RegExp.exec regex "123ba" in
              assert_result (RegExp.captures result) [| "a" |]);
          test "with m (multiline)" (fun () ->
              let regex = RegExp.compile "^d" "m" in
              let result = RegExp.exec regex "123bA" in
              assert_result (RegExp.captures result) [||];
              let result = RegExp.exec regex "123bA\n123" in
              assert_result (RegExp.captures result) [||];
              let result = RegExp.exec regex "david" in
              assert_result (RegExp.captures result) [| "d" |];
              let result = RegExp.exec regex "123bA\ndavid" in
              assert_result (RegExp.captures result) [| "d" |]);
          test "with g (global)" (fun () ->
              let regex = RegExp.compile "[0-9]+" "g" in
              let input = "abc00123xyz456_0" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "00123" |];
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "456" |];
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "0" |]);
          test "with y (sticky)" (fun () ->
              let regex = RegExp.compile "foo" "y" in
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
          test "groups" (fun () ->
              let regex = RegExp.compile "(xyz)" "" in
              let input = "xyz yz xyzx xzy" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "xyz"; "xyz" |]);
          test "index" (fun () ->
              let regex = RegExp.compile "World" "" in
              let input = "Hello World" in
              let result = RegExp.exec regex input in
              assert_int (RegExp.index result) 6);
          test "lastIndex and index" (fun () ->
              let regex = RegExp.compile "hello" "g" in
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
              let regex = RegExp.compile "(?<=\\b)[d-f]{3}" "" in
              let input = "def" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "def" |]);
          (* test "named groups?" *)
        ] );
    ]
