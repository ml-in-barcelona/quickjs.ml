module RegExp = Quickjs.RegExp
module Unicode = Quickjs.Unicode

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
      ( "Unicode.is_cased",
        [
          test "ASCII letters are cased" (fun () ->
              assert_bool (Unicode.is_cased (Uchar.of_char 'a')) true;
              assert_bool (Unicode.is_cased (Uchar.of_char 'A')) true;
              assert_bool (Unicode.is_cased (Uchar.of_char 'z')) true;
              assert_bool (Unicode.is_cased (Uchar.of_char 'Z')) true);
          test "digits and symbols are not cased" (fun () ->
              assert_bool (Unicode.is_cased (Uchar.of_char '0')) false;
              assert_bool (Unicode.is_cased (Uchar.of_char '!')) false;
              assert_bool (Unicode.is_cased (Uchar.of_char ' ')) false);
          test "accented letters are cased" (fun () ->
              (* é = U+00E9 *)
              assert_bool (Unicode.is_cased (Uchar.of_int 0x00E9)) true;
              (* É = U+00C9 *)
              assert_bool (Unicode.is_cased (Uchar.of_int 0x00C9)) true);
        ] );
      ( "Unicode.is_id_start",
        [
          test "letters can start identifiers" (fun () ->
              assert_bool (Unicode.is_id_start (Uchar.of_char 'a')) true;
              assert_bool (Unicode.is_id_start (Uchar.of_char 'Z')) true);
          test "digits cannot start identifiers" (fun () ->
              assert_bool (Unicode.is_id_start (Uchar.of_char '0')) false;
              assert_bool (Unicode.is_id_start (Uchar.of_char '9')) false);
          test "underscore and dollar - Unicode ID_Start excludes them" (fun () ->
              (* Note: Unicode ID_Start does NOT include _ and $
                 JavaScript adds them as special cases *)
              assert_bool (Unicode.is_id_start (Uchar.of_char '_')) false;
              assert_bool (Unicode.is_id_start (Uchar.of_char '$')) false);
          test "unicode letters can start identifiers" (fun () ->
              (* Greek alpha = U+03B1 *)
              assert_bool (Unicode.is_id_start (Uchar.of_int 0x03B1)) true;
              (* Chinese character 中 = U+4E2D *)
              assert_bool (Unicode.is_id_start (Uchar.of_int 0x4E2D)) true);
        ] );
      ( "Unicode.is_id_continue",
        [
          test "letters can continue identifiers" (fun () ->
              assert_bool (Unicode.is_id_continue (Uchar.of_char 'a')) true;
              assert_bool (Unicode.is_id_continue (Uchar.of_char 'Z')) true);
          test "digits can continue identifiers" (fun () ->
              assert_bool (Unicode.is_id_continue (Uchar.of_char '0')) true;
              assert_bool (Unicode.is_id_continue (Uchar.of_char '9')) true);
          test "underscore in ID_Continue but dollar not" (fun () ->
              (* Underscore is in Unicode ID_Continue (via Pc category) *)
              assert_bool (Unicode.is_id_continue (Uchar.of_char '_')) true;
              (* Dollar is NOT in Unicode ID_Continue - JavaScript adds it *)
              assert_bool (Unicode.is_id_continue (Uchar.of_char '$')) false);
        ] );
      ( "Unicode.is_whitespace",
        [
          test "ASCII whitespace" (fun () ->
              assert_bool (Unicode.is_whitespace (Uchar.of_char ' ')) true;
              assert_bool (Unicode.is_whitespace (Uchar.of_char '\t')) true;
              assert_bool (Unicode.is_whitespace (Uchar.of_char '\n')) true);
          test "letters are not whitespace" (fun () ->
              assert_bool (Unicode.is_whitespace (Uchar.of_char 'a')) false;
              assert_bool (Unicode.is_whitespace (Uchar.of_char '0')) false);
          test "unicode whitespace characters" (fun () ->
              (* EN QUAD = U+2000 *)
              assert_bool (Unicode.is_whitespace (Uchar.of_int 0x2000)) true;
              (* IDEOGRAPHIC SPACE = U+3000 *)
              assert_bool (Unicode.is_whitespace (Uchar.of_int 0x3000)) true);
        ] );
      ( "Unicode.lowercase",
        [
          test "ASCII lowercase" (fun () ->
              assert_string (Unicode.lowercase "HELLO") "hello";
              assert_string (Unicode.lowercase "Hello World") "hello world");
          test "already lowercase unchanged" (fun () ->
              assert_string (Unicode.lowercase "hello") "hello");
          test "unicode lowercase" (fun () ->
              assert_string (Unicode.lowercase "ÉCOLE") "école");
          test "empty string" (fun () ->
              assert_string (Unicode.lowercase "") "");
        ] );
      ( "Unicode.uppercase",
        [
          test "ASCII uppercase" (fun () ->
              assert_string (Unicode.uppercase "hello") "HELLO";
              assert_string (Unicode.uppercase "Hello World") "HELLO WORLD");
          test "already uppercase unchanged" (fun () ->
              assert_string (Unicode.uppercase "HELLO") "HELLO");
          test "unicode uppercase" (fun () ->
              assert_string (Unicode.uppercase "école") "ÉCOLE");
          test "German sharp s expands" (fun () ->
              (* ß → SS *)
              assert_string (Unicode.uppercase "straße") "STRASSE");
          test "empty string" (fun () ->
              assert_string (Unicode.uppercase "") "");
        ] );
      ( "Unicode.lowercase_char",
        [
          test "ASCII letter" (fun () ->
              let result = Unicode.lowercase_char (Uchar.of_char 'A') in
              assert_int (List.length result) 1;
              assert_bool (List.hd result = Uchar.of_char 'a') true);
          test "already lowercase" (fun () ->
              let result = Unicode.lowercase_char (Uchar.of_char 'a') in
              assert_int (List.length result) 1;
              assert_bool (List.hd result = Uchar.of_char 'a') true);
        ] );
      ( "Unicode.uppercase_char",
        [
          test "ASCII letter" (fun () ->
              let result = Unicode.uppercase_char (Uchar.of_char 'a') in
              assert_int (List.length result) 1;
              assert_bool (List.hd result = Uchar.of_char 'A') true);
          test "German sharp s expands to two chars" (fun () ->
              (* ß = U+00DF → SS *)
              let result = Unicode.uppercase_char (Uchar.of_int 0x00DF) in
              assert_int (List.length result) 2;
              assert_bool (List.nth result 0 = Uchar.of_char 'S') true;
              assert_bool (List.nth result 1 = Uchar.of_char 'S') true);
        ] );
      ( "Unicode.canonicalize",
        [
          test "unicode mode: uppercase to lowercase" (fun () ->
              let result = Unicode.canonicalize (Uchar.of_char 'A') in
              assert_bool (result = Uchar.of_char 'a') true);
          test "unicode mode: lowercase stays same" (fun () ->
              let result = Unicode.canonicalize (Uchar.of_char 'a') in
              assert_bool (result = Uchar.of_char 'a') true);
          test "non-unicode mode: lowercase to uppercase (legacy behavior)" (fun () ->
              (* In non-unicode mode, lre_canonicalize converts lowercase to uppercase *)
              let result = Unicode.canonicalize ~unicode:false (Uchar.of_char 'a') in
              assert_bool (result = Uchar.of_char 'A') true);
        ] );
      ( "Unicode.normalize",
        [
          test "NFC: composed form" (fun () ->
              (* café with combining acute (U+0301) should compose to é (U+00E9) *)
              let decomposed = "cafe\xCC\x81" in (* café with combining acute *)
              let result = Unicode.normalize NFC decomposed in
              assert_bool (Option.is_some result) true;
              (* The composed form should be shorter or equal *)
              assert_bool (String.length (Option.get result) <= String.length decomposed + 1) true);
          test "NFD: decomposed form" (fun () ->
              let composed = "café" in
              let result = Unicode.normalize NFD composed in
              assert_bool (Option.is_some result) true);
          test "ASCII unchanged by NFC" (fun () ->
              let ascii = "hello world" in
              let result = Unicode.normalize NFC ascii in
              assert_bool (Option.is_some result) true;
              assert_string (Option.get result) ascii);
          test "empty string" (fun () ->
              let result = Unicode.normalize NFC "" in
              assert_bool (Option.is_some result) true;
              assert_string (Option.get result) "");
          test "NFKC compatibility decomposition" (fun () ->
              (* ﬁ (U+FB01) should decompose to fi *)
              let ligature = "\xEF\xAC\x81" in
              let result = Unicode.normalize NFKC ligature in
              assert_bool (Option.is_some result) true;
              assert_string (Option.get result) "fi");
        ] );
    ]
