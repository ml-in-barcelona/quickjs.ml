(** TC39 Test262: RegExp flag property tests

    Based on:
    https://github.com/tc39/test262/tree/main/test/built-ins/RegExp/prototype/

    Tests for:
    - RegExp.prototype.flags (string accessor)
    - RegExp.prototype.global
    - RegExp.prototype.ignoreCase
    - RegExp.prototype.multiline
    - RegExp.prototype.dotAll
    - RegExp.prototype.sticky
    - RegExp.prototype.unicode *)

module RegExp = Quickjs.RegExp

(* ===================================================================
   RegExp.prototype.flags - string containing all set flags
   =================================================================== *)

let flags_empty () =
  (* No flags *)
  let re = regexp_compile "abc" ~flags:"" in
  assert_string (RegExp.flags re) ""

let flags_single () =
  (* Single flags *)
  let re_g = regexp_compile "abc" ~flags:"g" in
  assert_string (RegExp.flags re_g) "g";

  let re_i = regexp_compile "abc" ~flags:"i" in
  assert_string (RegExp.flags re_i) "i";

  let re_m = regexp_compile "abc" ~flags:"m" in
  assert_string (RegExp.flags re_m) "m"

let flags_multiple () =
  (* Multiple flags - order is normalized in flags string *)
  let re = regexp_compile "abc" ~flags:"gi" in
  let flags = RegExp.flags re in
  assert_bool (String.contains flags 'g') true;
  assert_bool (String.contains flags 'i') true

let flags_all () =
  (* All flags *)
  let re = regexp_compile "abc" ~flags:"gimsuy" in
  let flags = RegExp.flags re in
  assert_bool (String.contains flags 'g') true;
  assert_bool (String.contains flags 'i') true;
  assert_bool (String.contains flags 'm') true;
  assert_bool (String.contains flags 's') true;
  assert_bool (String.contains flags 'u') true;
  assert_bool (String.contains flags 'y') true

(* ===================================================================
   RegExp.prototype.global
   =================================================================== *)

let global_true () =
  let re = regexp_compile "abc" ~flags:"g" in
  assert_bool (RegExp.global re) true

let global_false () =
  let re = regexp_compile "abc" ~flags:"" in
  assert_bool (RegExp.global re) false

let global_with_others () =
  let re = regexp_compile "abc" ~flags:"gi" in
  assert_bool (RegExp.global re) true;

  let re2 = regexp_compile "abc" ~flags:"im" in
  assert_bool (RegExp.global re2) false

(* ===================================================================
   RegExp.prototype.ignoreCase
   =================================================================== *)

let ignorecase_true () =
  let re = regexp_compile "abc" ~flags:"i" in
  assert_bool (RegExp.ignorecase re) true

let ignorecase_false () =
  let re = regexp_compile "abc" ~flags:"" in
  assert_bool (RegExp.ignorecase re) false

let ignorecase_with_others () =
  let re = regexp_compile "abc" ~flags:"gi" in
  assert_bool (RegExp.ignorecase re) true;

  let re2 = regexp_compile "abc" ~flags:"gm" in
  assert_bool (RegExp.ignorecase re2) false

(* ===================================================================
   RegExp.prototype.multiline
   =================================================================== *)

let multiline_true () =
  let re = regexp_compile "^abc" ~flags:"m" in
  assert_bool (RegExp.multiline re) true

let multiline_false () =
  let re = regexp_compile "^abc" ~flags:"" in
  assert_bool (RegExp.multiline re) false

let multiline_with_others () =
  let re = regexp_compile "^abc" ~flags:"gm" in
  assert_bool (RegExp.multiline re) true;

  let re2 = regexp_compile "^abc" ~flags:"gi" in
  assert_bool (RegExp.multiline re2) false

(* ===================================================================
   RegExp.prototype.dotAll
   =================================================================== *)

let dotall_true () =
  let re = regexp_compile "a.b" ~flags:"s" in
  assert_bool (RegExp.dotall re) true

let dotall_false () =
  let re = regexp_compile "a.b" ~flags:"" in
  assert_bool (RegExp.dotall re) false

let dotall_behavior () =
  (* Without dotall, . doesn't match newlines *)
  let re_no_s = regexp_compile "a.b" ~flags:"" in
  assert_bool (RegExp.test re_no_s "a\nb") false;

  (* With dotall, . matches newlines *)
  let re_s = regexp_compile "a.b" ~flags:"s" in
  assert_bool (RegExp.test re_s "a\nb") true

(* ===================================================================
   RegExp.prototype.sticky
   =================================================================== *)

let sticky_true () =
  let re = regexp_compile "abc" ~flags:"y" in
  assert_bool (RegExp.sticky re) true

let sticky_false () =
  let re = regexp_compile "abc" ~flags:"" in
  assert_bool (RegExp.sticky re) false

let sticky_behavior () =
  (* Sticky only matches at lastIndex position *)
  let re = regexp_compile "a" ~flags:"y" in

  (* At position 0, matches first 'a' in "abc" *)
  assert_bool (RegExp.test re "abc") true;

  (* After match, lastIndex advances *)
  assert_int (RegExp.lastIndex re) 1;

  (* Next test fails because 'b' is at position 1 *)
  assert_bool (RegExp.test re "abc") false

(* ===================================================================
   RegExp.prototype.unicode
   =================================================================== *)

let unicode_true () =
  let re = regexp_compile "abc" ~flags:"u" in
  assert_bool (RegExp.unicode re) true

let unicode_false () =
  let re = regexp_compile "abc" ~flags:"" in
  assert_bool (RegExp.unicode re) false

(* ===================================================================
   Combined flag behavior tests
   =================================================================== *)

let flags_gi_behavior () =
  (* Global + ignoreCase *)
  let re = regexp_compile "a" ~flags:"gi" in
  let input = "AaAa" in

  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) false (* exhausted *)

let flags_gm_behavior () =
  (* Global + multiline *)
  let re = regexp_compile "^a" ~flags:"gm" in
  let input = "a\na\na" in

  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) true;
  assert_bool (RegExp.test re input) false

let flags_gy_behavior () =
  (* Global + sticky - sticky takes precedence *)
  let re = regexp_compile "a" ~flags:"gy" in

  assert_bool (RegExp.test re "aaa") true;
  assert_int (RegExp.lastIndex re) 1;

  assert_bool (RegExp.test re "aaa") true;
  assert_int (RegExp.lastIndex re) 2;

  assert_bool (RegExp.test re "aaa") true;
  assert_int (RegExp.lastIndex re) 3;

  assert_bool (RegExp.test re "aaa") false;
  assert_int (RegExp.lastIndex re) 0

(* ===================================================================
   Flag accessor consistency
   =================================================================== *)

let flags_consistency () =
  (* All flag accessors should be consistent with flags string *)
  let re = regexp_compile "abc" ~flags:"gimsy" in
  let flags_str = RegExp.flags re in

  assert_bool (RegExp.global re) (String.contains flags_str 'g');
  assert_bool (RegExp.ignorecase re) (String.contains flags_str 'i');
  assert_bool (RegExp.multiline re) (String.contains flags_str 'm');
  assert_bool (RegExp.dotall re) (String.contains flags_str 's');
  assert_bool (RegExp.sticky re) (String.contains flags_str 'y')

let tests =
  [
    (* flags string *)
    test "flags: empty" flags_empty;
    test "flags: single" flags_single;
    test "flags: multiple" flags_multiple;
    test "flags: all" flags_all;
    (* global *)
    test "global: true" global_true;
    test "global: false" global_false;
    test "global: with others" global_with_others;
    (* ignoreCase *)
    test "ignoreCase: true" ignorecase_true;
    test "ignoreCase: false" ignorecase_false;
    test "ignoreCase: with others" ignorecase_with_others;
    (* multiline *)
    test "multiline: true" multiline_true;
    test "multiline: false" multiline_false;
    test "multiline: with others" multiline_with_others;
    (* dotAll *)
    test "dotAll: true" dotall_true;
    test "dotAll: false" dotall_false;
    test "dotAll: behavior" dotall_behavior;
    (* sticky *)
    test "sticky: true" sticky_true;
    test "sticky: false" sticky_false;
    test "sticky: behavior" sticky_behavior;
    (* unicode *)
    test "unicode: true" unicode_true;
    test "unicode: false" unicode_false;
    (* combined *)
    test "combined: gi" flags_gi_behavior;
    test "combined: gm" flags_gm_behavior;
    test "combined: gy" flags_gy_behavior;
    test "combined: consistency" flags_consistency;
  ]
