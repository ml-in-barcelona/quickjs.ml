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
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let cps = ref [] in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        cps := Uchar.to_int u :: !cps;
        loop ()
    | `End -> Array.of_list (List.rev !cps)
    | `Malformed _ ->
        (* Replace malformed sequences with replacement character *)
        cps := 0xFFFD :: !cps;
        loop ()
    | `Await -> assert false
  in
  loop ()

(* Convert array of code points to UTF-8 string *)
let codepoints_to_utf8 cps =
  let buf = Buffer.create (Array.length cps * 4) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer buf) in
  Array.iter
    (fun cp ->
      let u =
        if cp >= 0 && cp <= 0x10FFFF then Uchar.of_int cp else Uchar.rep
      in
      ignore (Uutf.encode encoder (`Uchar u)))
    cps;
  ignore (Uutf.encode encoder `End);
  Buffer.contents buf

(* Character Classification *)

let is_cased c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  Libunicode.is_cased cp <> 0

let is_case_ignorable c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  Libunicode.is_case_ignorable cp <> 0

let is_id_start c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  Libunicode.is_id_start cp <> 0

let is_id_continue c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  Libunicode.is_id_continue cp <> 0

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

let case_conv_string conv_type s =
  let cps = utf8_to_codepoints s in
  let res = Ctypes.CArray.make Ctypes.uint32_t lre_cc_res_len_max in
  let res_ptr = Ctypes.CArray.start res in
  let result = Buffer.create (Stdlib.String.length s * 2) in
  let encoder = Uutf.encoder `UTF_8 (`Buffer result) in
  Array.iter
    (fun cp_int ->
      let cp = Unsigned.UInt32.of_int cp_int in
      let count = Libunicode.case_conv res_ptr cp conv_type in
      for i = 0 to count - 1 do
        let code = Unsigned.UInt32.to_int (Ctypes.CArray.get res i) in
        let u =
          if code >= 0 && code <= 0x10FFFF then Uchar.of_int code else Uchar.rep
        in
        ignore (Uutf.encode encoder (`Uchar u))
      done)
    cps;
  ignore (Uutf.encode encoder `End);
  Buffer.contents result

let uppercase s = case_conv_string 0 s
let lowercase s = case_conv_string 1 s

(* Canonicalization for regex matching *)

let canonicalize ?(unicode = true) c =
  let cp = Unsigned.UInt32.of_int (Uchar.to_int c) in
  let is_unicode = if unicode then 1 else 0 in
  let result = Libunicode.canonicalize cp is_unicode in
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
