(** JavaScript-compatible string-to-double conversion

    This module provides bindings to QuickJS's dtoa library for string-to-double
    conversion with JavaScript-compatible semantics. *)

(* JS_ATOD flags from dtoa.h *)
let js_atod_int_only = 1 lsl 0
let js_atod_accept_bin_oct = 1 lsl 1
let js_atod_accept_legacy_octal = 1 lsl 2
let js_atod_accept_underscores = 1 lsl 3

(** Parsing options for string-to-double conversion *)

type options = {
  radix : int;  (** 0 = auto-detect, 2-36 = fixed radix *)
  int_only : bool;  (** Only accept integers (no dot, no exponent) *)
  accept_bin_oct : bool;  (** Accept 0b and 0o prefixes when radix=0 *)
  accept_legacy_octal : bool;  (** Accept 0777 style (Annex B) when radix=0 *)
  accept_underscores : bool;  (** Accept 1_000_000 as digit separator *)
}

let default_options =
  {
    radix = 10;
    int_only = false;
    accept_bin_oct = false;
    accept_legacy_octal = false;
    accept_underscores = false;
  }

let js_options =
  {
    radix = 0;
    (* auto-detect *)
    int_only = false;
    accept_bin_oct = true;
    accept_legacy_octal = false;
    accept_underscores = false;
  }

let validate_radix radix =
  if radix <> 0 && (radix < 2 || radix > 36) then
    invalid_arg
      (Printf.sprintf
         "Atod.parse: radix must be 0 (auto) or between 2 and 36, got %d" radix)

let options_to_flags options =
  let flags = 0 in
  let flags =
    if options.int_only then flags lor js_atod_int_only else flags
  in
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

let parse ?(options = default_options) str =
  validate_radix options.radix;
  let flags = options_to_flags options in
  (* Allocate pnext pointer *)
  let pnext =
    Ctypes.allocate (Ctypes.ptr Ctypes.char)
      (Ctypes.from_voidp Ctypes.char Ctypes.null)
  in
  (* Allocate temp memory (JSATODTempMem is 27 * 8 = 216 bytes) *)
  let tmp_mem = Ctypes.allocate_n Ctypes.uint64_t ~count:27 in
  let tmp_mem_ptr = Ctypes.to_voidp tmp_mem in
  (* Call js_atod *)
  let result =
    Bindings.C.Functions.js_atod str pnext options.radix flags tmp_mem_ptr
  in
  (* Check if we consumed the whole string by checking if next pointer advanced *)
  let next_ptr = Ctypes.( !@ ) pnext in
  if Ctypes.is_null next_ptr then None
  else if
    Float.is_nan result
    && not (String.length str > 0 && (str.[0] = 'N' || str.[0] = 'n'))
  then
    (* NaN result usually means parse error, unless input was actually "NaN" *)
    None
  else Some result

let parse_partial ?(options = default_options) str =
  validate_radix options.radix;
  let flags = options_to_flags options in
  (* Allocate pnext pointer *)
  let pnext =
    Ctypes.allocate (Ctypes.ptr Ctypes.char)
      (Ctypes.from_voidp Ctypes.char Ctypes.null)
  in
  (* Allocate temp memory *)
  let tmp_mem = Ctypes.allocate_n Ctypes.uint64_t ~count:27 in
  let tmp_mem_ptr = Ctypes.to_voidp tmp_mem in
  (* Call js_atod *)
  let result =
    Bindings.C.Functions.js_atod str pnext options.radix flags tmp_mem_ptr
  in
  let next_ptr = Ctypes.( !@ ) pnext in
  if Ctypes.is_null next_ptr then None
  else
    (* Calculate offset from original string *)
    let str_start = Ctypes.coerce Ctypes.string (Ctypes.ptr Ctypes.char) str in
    let offset = Ctypes.ptr_diff str_start next_ptr in
    if offset <= 0 then None
    else
      let remaining = String.sub str offset (String.length str - offset) in
      Some (result, remaining)

