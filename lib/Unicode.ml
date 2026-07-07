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

(* QuickJS's folding table maps a few capitals through their lowercase form
   (their simple case folding), one step short of Unicode full case folding:
   e.g. U+1E9E -> U+00DF (spec: "ss") and U+1F88 -> U+1F80 (spec: U+1F00
   U+03B9). Unicode case folding is idempotent, so refolding every output
   until it is stable restores the spec mapping - at most two steps with the
   current table; [fuel] guards against cycles in future tables. *)
let fold_case_char c =
  let rec refold fuel c =
    match case_conv_char 2 c with
    | [ folded ] when Uchar.equal folded c -> [ c ]
    | outputs when fuel = 0 -> outputs
    | outputs -> List.concat_map (refold (fuel - 1)) outputs
  in
  refold 4 c

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

(* Folding needs the refold treatment (see fold_case_char), so it does not
   share case_conv_string's single-pass loop. Folding is also
   context-independent: no Final_Sigma rule applies. *)
let fold_case s =
  let len = Stdlib.String.length s in
  let buf = Stdlib.Buffer.create (len * 2) in
  let rec loop i =
    if i >= len then Stdlib.Buffer.contents buf
    else begin
      let d = Stdlib.String.get_utf_8_uchar s i in
      List.iter
        (Stdlib.Buffer.add_utf_8_uchar buf)
        (fold_case_char (Uchar.utf_decode_uchar d));
      loop (i + Uchar.utf_decode_length d)
    end
  in
  loop 0

(* Canonicalization for regex matching *)

let canonicalize ?(unicode = true) c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  let result = Libunicode.canonicalize cp unicode in
  if result >= 0 && result <= 0x10FFFF then Uchar.of_int result else c

(* Character Sets - Script / General_Category / binary properties *)

module CharSet = struct
  (* Inclusive code point ranges, sorted by increasing value and disjoint.
     Stored as ints (not Uchar.t) because some sets contain surrogate code
     points, e.g. General_Category Cs. *)
  type t = { ranges : (int * int) array }

  let ranges t = Array.copy t.ranges

  let mem c t =
    let cp = Uchar.to_int c in
    let lo = ref 0 and hi = ref (Array.length t.ranges - 1) in
    let found = ref false in
    while (not !found) && !lo <= !hi do
      let mid = (!lo + !hi) / 2 in
      let first, last = t.ranges.(mid) in
      if cp < first then hi := mid - 1
      else if cp > last then lo := mid + 1
      else found := true
    done;
    !found
end

(* kind: 0 = Script, 1 = Script_Extensions, 2 = General_Category,
   3 = binary property (see unicode_char_range_shim) *)
let char_range_lookup kind name =
  let dst =
    Ctypes.allocate
      (Ctypes.ptr Ctypes.uint32_t)
      (Ctypes.from_voidp Ctypes.uint32_t Ctypes.null)
  in
  let len = Libunicode.char_range kind name dst in
  if len = -2 then None
  else if len < 0 then raise Out_of_memory
  else begin
    let points = Ctypes.( !@ ) dst in
    (* The buffer holds half-open intervals [points.(2i), points.(2i+1));
       store them as inclusive ranges. *)
    let ranges =
      Array.init (len / 2) (fun i ->
          let first =
            Unsigned.UInt32.to_int
              (Ctypes.( !@ ) (Ctypes.( +@ ) points (2 * i)))
          in
          let last =
            Unsigned.UInt32.to_int
              (Ctypes.( !@ ) (Ctypes.( +@ ) points ((2 * i) + 1)))
          in
          (first, last - 1))
    in
    Libunicode.char_range_free points;
    Some { CharSet.ranges }
  end

let script ?(extensions = false) name =
  char_range_lookup (if extensions then 1 else 0) name

let general_category name = char_range_lookup 2 name
let binary_property name = char_range_lookup 3 name

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
