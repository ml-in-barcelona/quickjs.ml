(** JavaScript Global object functions

    This module contains global functions from the JavaScript specification like
    parseFloat and parseInt. *)

module Atod = Quickjs_c.Atod
module Libunicode = Quickjs_c.Libunicode

(* JS_ATOD flags from dtoa.h *)
let js_atod_int_only = 1 lsl 0
let js_atod_accept_bin_oct = 1 lsl 1
let js_atod_accept_legacy_octal = 1 lsl 2
let js_atod_accept_underscores = 1 lsl 3

(* Workaround for QuickJS bug: incomplete exponents like "1e", "1e+", "1e-"
   return NaN instead of backtracking to parse "1".
   See: https://github.com/quickjs-ng/quickjs/issues/1259
   JavaScript spec: parseFloat("1e") === 1, parseFloat("1e+") === 1, parseFloat("1e-") === 1

   This function strips incomplete exponent suffixes from the end of a string.
   Only applies to radix 10 (decimal) parsing. *)
let strip_incomplete_exponent str =
  let len = Stdlib.String.length str in
  if len = 0 then str
  else
    (* Find the last non-whitespace character position *)
    let rec find_end i =
      if i < 0 then -1
      else
        let c = Stdlib.String.get str i in
        if c = ' ' || c = '\t' || c = '\n' || c = '\r' then find_end (i - 1)
        else i
    in
    let last_idx = find_end (len - 1) in
    if last_idx < 0 then str
    else
      let last_char = Stdlib.String.get str last_idx in
      (* Check for incomplete exponent: ends with e, E, e+, E+, e-, E- *)
      if last_char = 'e' || last_char = 'E' then
        (* "1e" -> "1" *)
        Stdlib.String.sub str 0 last_idx
      else if (last_char = '+' || last_char = '-') && last_idx > 0 then
        let prev_char = Stdlib.String.get str (last_idx - 1) in
        if prev_char = 'e' || prev_char = 'E' then
          (* "1e+" or "1e-" -> "1" *)
          Stdlib.String.sub str 0 (last_idx - 1)
        else str
      else str

type parse_options = {
  radix : int;
  int_only : bool;
  accept_bin_oct : bool;
  accept_legacy_octal : bool;
  accept_underscores : bool;
}

let default_parse_options =
  {
    radix = 10;
    int_only = false;
    accept_bin_oct = false;
    accept_legacy_octal = false;
    accept_underscores = false;
  }

let js_number_options =
  {
    radix = 0;
    int_only = false;
    accept_bin_oct = true;
    accept_legacy_octal = false;
    accept_underscores = false;
  }

let validate_radix radix =
  if radix <> 0 && (radix < 2 || radix > 36) then
    invalid_arg
      (Printf.sprintf
         "Global.parse_float: radix must be 0 (auto) or between 2 and 36, got \
          %d"
         radix)

let options_to_flags options =
  let flags = 0 in
  let flags = if options.int_only then flags lor js_atod_int_only else flags in
  let flags =
    if options.accept_bin_oct then flags lor js_atod_accept_bin_oct else flags
  in
  let flags =
    if options.accept_legacy_octal then flags lor js_atod_accept_legacy_octal
    else flags
  in
  let flags =
    if options.accept_underscores then flags lor js_atod_accept_underscores
    else flags
  in
  flags

(* Number of leading bytes of [str] that are JavaScript whitespace
   (WhiteSpace or LineTerminator, the set skipped by parseInt/parseFloat).
   UTF-8 aware: skips non-ASCII whitespace like NBSP or U+2028 as well. *)
let js_whitespace_prefix str =
  let len = Stdlib.String.length str in
  let rec skip i =
    if i >= len then i
    else
      let d = Stdlib.String.get_utf_8_uchar str i in
      let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
      if Libunicode.is_space code then skip (i + Uchar.utf_decode_length d)
      else i
  in
  skip 0

(* Parse the longest numeric prefix of [str]. Returns the value and the
   number of bytes consumed, or [None] when no prefix parses. js_atod never
   returns NaN for a successfully parsed input, so NaN means failure. *)
let atod_prefix ~options str =
  let flags = options_to_flags options in
  let poffset = Ctypes.allocate Ctypes.int 0 in
  let tmp_mem = Ctypes.allocate_n Ctypes.uint64_t ~count:27 in
  let tmp_mem_ptr = Ctypes.to_voidp tmp_mem in
  let result = Atod.parse str poffset options.radix flags tmp_mem_ptr in
  if Float.is_nan result then None else Some (result, Ctypes.( !@ ) poffset)

let parse_float_raw ~options str =
  match atod_prefix ~options str with
  | Some (value, _consumed) -> Some value
  | None ->
      (* Workaround for QuickJS incomplete exponent bug.
         Only applies to decimal (radix 10 or auto-detect radix 0) *)
      if options.radix = 10 || options.radix = 0 then
        let stripped = strip_incomplete_exponent str in
        if stripped <> str && Stdlib.String.length stripped > 0 then
          match atod_prefix ~options stripped with
          | Some (value, _consumed) -> Some value
          | None -> None
        else None
      else None

let parse_float ?(options = default_parse_options) str =
  validate_radix options.radix;
  (* JavaScript's parseFloat skips leading whitespace *)
  let start = js_whitespace_prefix str in
  let trimmed =
    Stdlib.String.sub str start (Stdlib.String.length str - start)
  in
  parse_float_raw ~options trimmed

let parse_float_partial ?(options = default_parse_options) str =
  validate_radix options.radix;
  let start = js_whitespace_prefix str in
  let trimmed =
    Stdlib.String.sub str start (Stdlib.String.length str - start)
  in
  match atod_prefix ~options trimmed with
  | None -> None
  | Some (value, consumed) ->
      let remaining =
        Stdlib.String.sub trimmed consumed
          (Stdlib.String.length trimmed - consumed)
      in
      Some (value, remaining)

(* Exact float bound of the OCaml int range: 2^(int_size-1). A float f is
   losslessly convertible to int iff -bound <= f < bound (any float strictly
   below bound is at most max_int; -bound itself is min_int). *)
let int_bound = Stdlib.ldexp 1.0 (Sys.int_size - 1)

let parse_int ?(radix = 0) str =
  (* Radix must be 0 (auto-detect, the JavaScript default) or 2-36 *)
  if radix <> 0 && (radix < 2 || radix > 36) then None
  else
    let ws_offset = js_whitespace_prefix str in
    let str_len = Stdlib.String.length str in
    if ws_offset >= str_len then None
    else
      let trimmed = Stdlib.String.sub str ws_offset (str_len - ws_offset) in
      (* int_only: no dot, no exponent. Note: JavaScript's parseInt only
         auto-detects the 0x/0X prefix (radix 16), never 0b/0o, so
         JS_ATOD_ACCEPT_BIN_OCT must not be set. *)
      let options = { default_parse_options with radix; int_only = true } in
      match atod_prefix ~options trimmed with
      | None -> None
      | Some (result, _consumed) ->
          if not (Float.is_finite result) then None
          else if result >= int_bound || result < -.int_bound then
            (* Out of OCaml int range: refuse to return a corrupted value *)
            None
          else Some (int_of_float result)
