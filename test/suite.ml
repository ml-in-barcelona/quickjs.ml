open Quickjs

(* let suites = Mt.[
     "captures", (fun _ ->
       let re = [%re "/(\\d+)-(?:(\\d+))?/g"] in
       let str = "3-" in
       match re |. Js.Re.exec_ str with
         | Some result ->
           let defined = (Js.Re.captures result).(1) in
           let undefined = (Js.Re.captures result).(2) in
           Eq((Js.Nullable.return "3", Js.Nullable.null), (defined, undefined))
         | None -> Fail()
     );

     "fromString", (fun _ ->
       (* From the example in js_re.mli *)
       let contentOf tag xmlString =
         Js.Re.fromString ("<" ^ tag ^ ">(.*?)<\\/" ^ tag ^">")
           |. Js.Re.exec_ xmlString
           |. function
             | Some result -> Js.Nullable.toOption (Js.Re.captures result).(1)
             | None -> None in
       Eq (contentOf "div" "<div>Hi</div>", Some "Hi")
     );

     "exec_literal", (fun _ ->
       match [%re "/[^.]+/"] |. Js.Re.exec_ "http://xxx.domain.com" with
       | Some res ->
         Eq(Js.Nullable.return "http://xxx", (Js.Re.captures res).(0))
       | None ->
         FailWith "regex should match"
     );

     "exec_no_match", (fun _ ->
       match [%re "/https:\\/\\/(.*)/"] |. Js.Re.exec_ "http://xxx.domain.com" with
       | Some _ ->  FailWith "regex should not match"
       | None -> Ok true
     );

     "test_str", (fun _ ->
       let res = "foo"
         |. Js.Re.fromString
         |. Js.Re.test_ "#foo#" in

       Eq(true, res)
     );

     "fromStringWithFlags", (fun _ ->
       let res = Js.Re.fromStringWithFlags "foo" ~flags:"g" in

       Eq(true, res |. Js.Re.global)
     );
     "result_index", (fun _ ->
       match "zbar" |. Js.Re.fromString |. Js.Re.exec_ "foobarbazbar" with
       | Some res ->
         Eq(8, res |> Js.Re.index)
       | None ->
         Fail ()
     );
     "result_input", (fun _ ->
       let input = "foobar" in

       match [%re "/foo/g"] |. Js.Re.exec_ input with
       | Some res ->
         Eq(input,  res |> Js.Re.input)
       | None ->
         Fail ()
     );

     (* es2015 *)
     "t_flags", (fun _ ->
       Eq("gi", [%re "/./ig"] |. Js.Re.flags)
     );

     "t_global", (fun _ ->
       Eq(true, [%re "/./ig"] |. Js.Re.global)
     );
     "t_ignoreCase", (fun _ ->
       Eq(true, [%re "/./ig"] |. Js.Re.ignoreCase)
     );
     "t_lastIndex", (fun _ ->
       let re = [%re "/na/g"] in
       let _ = re |. Js.Re.exec_ "banana" in     (* Caml_option.null_to_opt post operation is not dropped in 4.06 which seems to be reduandant *)
       Eq(4,  re |. Js.Re.lastIndex)
     );
     "t_setLastIndex", (fun _ ->
       let re = [%re "/na/g"] in

       let before = Js.Re.lastIndex re in
       let () = Js.Re.setLastIndex re 42 in
       let after = Js.Re.lastIndex re in

       Eq((0, 42),  (before, after))
     );
     "t_multiline", (fun _ ->
       Eq(false, [%re "/./ig"] |. Js.Re.multiline)
     );
     "t_source", (fun _ ->
       Eq("f.+o", [%re "/f.+o/ig"] |. Js.Re.source)
     );

     (* es2015 *)
     "t_sticky", (fun _ ->
       Eq(true, [%re "/./yg"] |. Js.Re.sticky)
     );
     "t_unicode", (fun _ ->
       Eq(false, [%re "/./yg"] |. Js.Re.unicode)
     );
   ]

   ;; Mt.from_pair_suites __MODULE__ suites *)

let test title fn = Alcotest.test_case title `Quick fn

let assert_result left right =
  Alcotest.(check (array string)) "match" right left

let assert_int left right = Alcotest.(check int) "match" right left
let assert_string left right = Alcotest.(check string) "match" right left

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
          (* https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/lookBehind/word-boundary.js *)
          test "groups with (?: )" (fun () ->
              let regex = RegExp.compile "(?<=\\b)[d-f]{3}" "" in
              let input = "def" in
              let result = RegExp.exec regex input in
              assert_result (RegExp.captures result) [| "def" |]);
          (* test "named groups?" *)
        ] );
    ]
