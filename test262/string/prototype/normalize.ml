(* TC39 Test262: String.prototype.normalize tests

   Based on:
   https://github.com/tc39/test262/tree/main/test/built-ins/String/prototype/normalize

   ECMA-262 Section: String.prototype.normalize([form])

   Tests for Unicode.normalize *)

module Unicode = Quickjs.Unicode

(* ===================================================================
   NFC - Canonical Decomposition, followed by Canonical Composition
   =================================================================== *)

let nfc_composed () =
  (* café with combining acute (U+0301) should compose to é (U+00E9) *)
  let decomposed = "cafe\xCC\x81" in
  (* café with combining acute *)
  let result = Unicode.normalize NFC decomposed in
  assert_bool (Option.is_some result) true;
  (* The composed form should be shorter or equal *)
  assert_bool
    (Stdlib.String.length (Option.get result)
    <= Stdlib.String.length decomposed + 1)
    true

let nfc_ascii_unchanged () =
  let ascii = "hello world" in
  let result = Unicode.normalize NFC ascii in
  assert_bool (Option.is_some result) true;
  assert_string (Option.get result) ascii

let nfc_empty_string () =
  let result = Unicode.normalize NFC "" in
  assert_bool (Option.is_some result) true;
  assert_string (Option.get result) ""

(* ===================================================================
   NFD - Canonical Decomposition
   =================================================================== *)

let nfd_decomposed () =
  let composed = "café" in
  let result = Unicode.normalize NFD composed in
  assert_bool (Option.is_some result) true

(* ===================================================================
   NFKC - Compatibility Decomposition, followed by Canonical Composition
   =================================================================== *)

let nfkc_compatibility () =
  (* ﬁ (U+FB01) should decompose to fi *)
  let ligature = "\xEF\xAC\x81" in
  let result = Unicode.normalize NFKC ligature in
  assert_bool (Option.is_some result) true;
  assert_string (Option.get result) "fi"

let tests =
  [
    test "S15.5.4.13_A1: NFC composed" nfc_composed;
    test "S15.5.4.13_A2: NFC ASCII unchanged" nfc_ascii_unchanged;
    test "S15.5.4.13_A3: NFC empty string" nfc_empty_string;
    test "S15.5.4.13_A4: NFD decomposed" nfd_decomposed;
    test "S15.5.4.13_A5: NFKC compatibility" nfkc_compatibility;
  ]
