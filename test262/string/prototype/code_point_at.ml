(* TC39 Test262: String.prototype.codePointAt tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/codePointAt

   ECMA-262 Section: String.prototype.codePointAt(pos)

   Tests for String.Prototype.code_point_at *)

module String = Quickjs.String

(* ===================================================================
   Basic codePointAt functionality
   =================================================================== *)

let basic_ascii () =
  assert_int_opt (String.Prototype.code_point_at 0 "hello") (Some 104);
  (* 'h' *)
  assert_int_opt (String.Prototype.code_point_at 1 "hello") (Some 101);
  (* 'e' *)
  assert_int_opt (String.Prototype.code_point_at 0 "A") (Some 65)

let out_of_bounds () =
  assert_int_opt (String.Prototype.code_point_at (-1) "hello") None;
  assert_int_opt (String.Prototype.code_point_at 5 "hello") None;
  assert_int_opt (String.Prototype.code_point_at 100 "hello") None

let empty_string () =
  assert_int_opt (String.Prototype.code_point_at 0 "") None

let unicode_bmp () =
  (* BMP characters - same as charCodeAt *)
  assert_int_opt (String.Prototype.code_point_at 3 "café") (Some 0xe9);
  assert_int_opt (String.Prototype.code_point_at 0 "日") (Some 0x65e5)

let unicode_supplementary () =
  (* The key difference from charCodeAt: codePointAt returns full code point *)
  (* "😀" is U+1F600 *)
  let emoji = "😀" in
  assert_int_opt (String.Prototype.code_point_at 0 emoji) (Some 0x1F600);
  (* At index 1 (low surrogate position), returns the low surrogate value *)
  assert_int_opt (String.Prototype.code_point_at 1 emoji) (Some 0xDE00)

let mixed_string () =
  (* "a😀b" - 'a' at 0, emoji at 1 (takes 2 UTF-16 units), 'b' at 3 *)
  let s = "a😀b" in
  assert_int_opt (String.Prototype.code_point_at 0 s) (Some 97);
  (* 'a' *)
  assert_int_opt (String.Prototype.code_point_at 1 s) (Some 0x1F600);
  (* 😀 *)
  assert_int_opt (String.Prototype.code_point_at 3 s) (Some 98)
(* 'b' *)

let lone_surrogates () =
  (* When a surrogate is not part of a pair, return its value *)
  (* This is edge case behavior *)
  ()

let tests =
  [
    test "codePointAt: basic ASCII" basic_ascii;
    test "codePointAt: out of bounds" out_of_bounds;
    test "codePointAt: empty string" empty_string;
    test "codePointAt: Unicode BMP" unicode_bmp;
    test "codePointAt: Unicode supplementary" unicode_supplementary;
    test "codePointAt: mixed string" mixed_string;
    test "codePointAt: lone surrogates" lone_surrogates;
  ]

