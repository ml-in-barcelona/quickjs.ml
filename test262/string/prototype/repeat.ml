(* TC39 Test262: String.prototype.repeat tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/repeat

   ECMA-262 Section: String.prototype.repeat(count)

   Tests for String.Prototype.repeat *)

module String = Quickjs.String

(* ===================================================================
   Basic repeat functionality
   =================================================================== *)

let basic_repeat () =
  assert_string (String.Prototype.repeat 3 "abc") "abcabcabc";
  assert_string (String.Prototype.repeat 2 "hello") "hellohello"

let zero_count () = assert_string (String.Prototype.repeat 0 "abc") ""
let one_count () = assert_string (String.Prototype.repeat 1 "abc") "abc"

let empty_string () =
  assert_string (String.Prototype.repeat 5 "") "";
  assert_string (String.Prototype.repeat 0 "") ""

let single_char () = assert_string (String.Prototype.repeat 5 "a") "aaaaa"

let unicode_repeat () =
  assert_string (String.Prototype.repeat 3 "日") "日日日";
  assert_string (String.Prototype.repeat 2 "😀") "😀😀"

let negative_count () =
  (* Negative count should raise or return error *)
  let result =
    try
      let _ = String.Prototype.repeat (-1) "abc" in
      false
    with _ -> true
  in
  assert_bool result true

let large_count () =
  (* Reasonable large count *)
  let result = String.Prototype.repeat 100 "a" in
  assert_int (Stdlib.String.length result) 100

let whitespace () =
  assert_string (String.Prototype.repeat 3 " ") "   ";
  assert_string (String.Prototype.repeat 2 "\t") "\t\t"

let multiline () = assert_string (String.Prototype.repeat 2 "a\nb") "a\nba\nb"

let tests =
  [
    test "repeat: basic" basic_repeat;
    test "repeat: zero count" zero_count;
    test "repeat: one count" one_count;
    test "repeat: empty string" empty_string;
    test "repeat: single char" single_char;
    test "repeat: Unicode" unicode_repeat;
    test "repeat: negative count" negative_count;
    test "repeat: large count" large_count;
    test "repeat: whitespace" whitespace;
    test "repeat: multiline" multiline;
  ]
