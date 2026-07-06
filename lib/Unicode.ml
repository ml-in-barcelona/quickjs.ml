module Libunicode = Quickjs_c.Libunicode

type normalization = NFC | NFD | NFKC | NFKD

let normalization_to_int = function
  | NFC -> 0
  | NFD -> 1
  | NFKC -> 2
  | NFKD -> 3

(* LRE_CC_RES_LEN_MAX from libunicode.h - max code points from case conversion *)
let lre_cc_res_len_max = 3

(* Convert UTF-8 string to array of code points (uint32_t) *)
let utf8_to_codepoints s =
  let len = Stdlib.String.length s in
  let cps = ref [] in
  let rec loop i =
    if i >= len then Array.of_list (List.rev !cps)
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      cps := Uchar.to_int u :: !cps;
      loop (i + Uchar.utf_decode_length d)
  in
  loop 0

(* Convert array of code points to UTF-8 string *)
let codepoints_to_utf8 cps =
  let buf = Stdlib.Buffer.create (Array.length cps * 4) in
  Array.iter
    (fun cp ->
      let u =
        if cp >= 0 && cp <= 0x10FFFF then Uchar.of_int cp else Uchar.rep
      in
      Stdlib.Buffer.add_utf_8_uchar buf u)
    cps;
  Stdlib.Buffer.contents buf

(* Character Classification *)

let is_cased c = Libunicode.is_cased (Unsigned.UInt32.of_int (Uchar.to_int c))

let is_case_ignorable c =
  Libunicode.is_case_ignorable (Unsigned.UInt32.of_int (Uchar.to_int c))

let is_id_start c =
  Libunicode.is_id_start (Unsigned.UInt32.of_int (Uchar.to_int c))

let is_id_continue c =
  Libunicode.is_id_continue (Unsigned.UInt32.of_int (Uchar.to_int c))

let is_whitespace c =
  (* lre_is_space handles both ASCII and non-ASCII whitespace *)
  Libunicode.is_space (Uchar.to_int c)

(* Case Conversion - Single Character *)

let case_conv_char conv_type c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  let res = Ctypes.CArray.make Ctypes.uint32_t lre_cc_res_len_max in
  let res_ptr = Ctypes.CArray.start res in
  let count = Libunicode.case_conv res_ptr cp conv_type in
  let result = ref [] in
  for i = count - 1 downto 0 do
    let code = Unsigned.UInt32.to_int (Ctypes.CArray.get res i) in
    if code >= 0 && code <= 0x10FFFF then result := Uchar.of_int code :: !result
  done;
  !result

let uppercase_char c = case_conv_char 0 c
let lowercase_char c = case_conv_char 1 c

(* Case Conversion - Strings *)

let is_cased_cp cp = Libunicode.is_cased (Unsigned.UInt32.of_int cp)

let is_case_ignorable_cp cp =
  Libunicode.is_case_ignorable (Unsigned.UInt32.of_int cp)

(* Unicode SpecialCasing Final_Sigma condition: the sigma at [pos] is
   preceded by a cased character (skipping case-ignorable ones) and not
   followed by one. Port of test_final_sigma in quickjs.c. *)
let is_final_sigma cps pos =
  let len = Array.length cps in
  (* before: skip case-ignorable characters and require a cased one *)
  let rec before i =
    if i < 0 then false
    else if is_case_ignorable_cp cps.(i) then before (i - 1)
    else is_cased_cp cps.(i)
  in
  (* after: skip case-ignorable characters and require no cased one *)
  let rec after i =
    if i >= len then true
    else if is_case_ignorable_cp cps.(i) then after (i + 1)
    else not (is_cased_cp cps.(i))
  in
  before (pos - 1) && after (pos + 1)

let capital_sigma = 0x3A3
let final_small_sigma = 0x3C2

let case_conv_string conv_type s =
  let cps = utf8_to_codepoints s in
  let res = Ctypes.CArray.make Ctypes.uint32_t lre_cc_res_len_max in
  let res_ptr = Ctypes.CArray.start res in
  let result = Stdlib.Buffer.create (Stdlib.String.length s * 2) in
  let add_code code =
    let u =
      if code >= 0 && code <= 0x10FFFF then Uchar.of_int code else Uchar.rep
    in
    Stdlib.Buffer.add_utf_8_uchar result u
  in
  Array.iteri
    (fun i cp_int ->
      (* Context-sensitive lowercase of capital sigma at the end of a word
         (same special case as js_string_toLowerCase in quickjs.c) *)
      if conv_type = 1 && cp_int = capital_sigma && is_final_sigma cps i then
        add_code final_small_sigma
      else begin
        let count =
          Libunicode.case_conv res_ptr (Unsigned.UInt32.of_int cp_int) conv_type
        in
        for j = 0 to count - 1 do
          add_code (Unsigned.UInt32.to_int (Ctypes.CArray.get res j))
        done
      end)
    cps;
  Stdlib.Buffer.contents result

let uppercase s = case_conv_string 0 s
let lowercase s = case_conv_string 1 s

(* Canonicalization for regex matching *)

let canonicalize ?(unicode = true) c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  let result = Libunicode.canonicalize cp unicode in
  if result >= 0 && result <= 0x10FFFF then Uchar.of_int result else c

(* Normalization *)
let normalize form s =
  let cps = utf8_to_codepoints s in
  let len = Array.length cps in
  if len = 0 then Some ""
  else
    let src =
      Ctypes.CArray.of_list Ctypes.uint32_t
        (Array.to_list (Array.map Unsigned.UInt32.of_int cps))
    in
    let src_ptr = Ctypes.CArray.start src in
    let dst_ptr =
      Ctypes.allocate
        (Ctypes.ptr Ctypes.uint32_t)
        (Ctypes.from_voidp Ctypes.uint32_t Ctypes.null)
    in
    let n_type = normalization_to_int form in
    let result_len = Libunicode.normalize src_ptr len n_type dst_ptr in
    if result_len < 0 then None
    else
      let dst = Ctypes.(!@dst_ptr) in
      let result_cps = Array.make result_len 0 in
      for i = 0 to result_len - 1 do
        result_cps.(i) <- Unsigned.UInt32.to_int Ctypes.(!@(dst +@ i))
      done;
      Libunicode.normalize_free dst;
      Some (codepoints_to_utf8 result_cps)
