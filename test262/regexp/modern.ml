module RegExp = Quickjs.RegExp

let register_backtracking () =
  let re = regexp_compile "^(a{1,3})+b$" ~flags:"" in
  assert_match
    (RegExp.exec re (Stdlib.String.make 12 'a' ^ "b"))
    [| Stdlib.String.make 12 'a' ^ "b"; "aaa" |];
  assert_no_match (RegExp.exec re (Stdlib.String.make 12 'a' ^ "c"))

let modifiers () =
  let re = regexp_compile "^(?i:quick)(?-i:JS)$" ~flags:"" in
  assert_bool (RegExp.test re "QUICKJS") true;
  assert_bool (RegExp.test re "QUICKjs") false

let unicode_set_subtraction () =
  let re = regexp_compile "^[[a-z]--[aeiou]]+$" ~flags:"v" in
  assert_bool (RegExp.test re "rhythm") true;
  assert_bool (RegExp.test re "quick") false

let unicode_property_of_strings () =
  let re = regexp_compile "^\\p{RGI_Emoji_Flag_Sequence}$" ~flags:"v" in
  assert_bool (RegExp.test re "🇫🇷") true;
  assert_bool (RegExp.test re "🇫") false

let duplicate_named_groups () =
  let re = regexp_compile "^(?:(?<x>a)|(?<x>b))(?<y>c)$" ~flags:"d" in
  let result_a =
    match RegExp.exec re "ac" with
    | Some result -> result
    | None -> Alcotest.fail "expected first duplicate named group to match"
  in
  Alcotest.(check (list (pair string (option string))))
    "first alternative groups"
    [ ("x", Some "a"); ("y", Some "c") ]
    result_a.groups;
  Alcotest.(check (option (pair int int)))
    "first alternative indices"
    (Some (0, 1))
    (RegExp.group_indices "x" result_a);
  let result_b =
    match RegExp.exec re "bc" with
    | Some result -> result
    | None -> Alcotest.fail "expected second duplicate named group to match"
  in
  Alcotest.(check (list (pair string (option string))))
    "second alternative groups"
    [ ("x", Some "b"); ("y", Some "c") ]
    result_b.groups;
  Alcotest.(check (option (pair int int)))
    "second alternative indices"
    (Some (0, 1))
    (RegExp.group_indices "x" result_b);
  Alcotest.(check (option (pair int int)))
    "following group indices"
    (Some (1, 2))
    (RegExp.group_indices "y" result_b)

let duplicate_named_groups_same_scope_rejected () =
  match RegExp.compile "(?<x>a)(?<x>b)" ~flags:"" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected duplicate group names in one scope to fail"

let invalid_braced_hex_escape () =
  assert_invalid_escape (regexp_no_compile "\\x{41}" ~flags:"u")

let tests =
  [
    test "register-heavy backtracking" register_backtracking;
    test "scoped modifiers" modifiers;
    test "unicode set subtraction" unicode_set_subtraction;
    test "unicode property of strings" unicode_property_of_strings;
    test "duplicate named groups" duplicate_named_groups;
    test "same-scope duplicate named groups rejected"
      duplicate_named_groups_same_scope_rejected;
    test "braced hex escape rejected" invalid_braced_hex_escape;
  ]
