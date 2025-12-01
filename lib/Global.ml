(** JavaScript Global object functions

    This module contains global functions from the JavaScript specification like
    parseFloat and parseInt. *)

(* JS_ATOD flags from dtoa.h *)
let js_atod_int_only = 1 lsl 0
let js_atod_accept_bin_oct = 1 lsl 1
let js_atod_accept_legacy_octal = 1 lsl 2
let js_atod_accept_underscores = 1 lsl 3

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

let js_parse_options =
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

let parse_float ?(options = default_parse_options) str =
  validate_radix options.radix;
  let flags = options_to_flags options in
  let pnext =
    Ctypes.allocate (Ctypes.ptr Ctypes.char)
      (Ctypes.from_voidp Ctypes.char Ctypes.null)
  in
  let tmp_mem = Ctypes.allocate_n Ctypes.uint64_t ~count:27 in
  let tmp_mem_ptr = Ctypes.to_voidp tmp_mem in
  let result =
    Bindings.C.Functions.js_atod str pnext options.radix flags tmp_mem_ptr
  in
  let next_ptr = Ctypes.( !@ ) pnext in
  if Ctypes.is_null next_ptr then None
  else if
    Float.is_nan result
    && not
         (Stdlib.String.length str > 0
         && (Stdlib.String.get str 0 = 'N' || Stdlib.String.get str 0 = 'n'))
  then None
  else Some result

let parse_float_partial ?(options = default_parse_options) str =
  validate_radix options.radix;
  let flags = options_to_flags options in
  let pnext =
    Ctypes.allocate (Ctypes.ptr Ctypes.char)
      (Ctypes.from_voidp Ctypes.char Ctypes.null)
  in
  let tmp_mem = Ctypes.allocate_n Ctypes.uint64_t ~count:27 in
  let tmp_mem_ptr = Ctypes.to_voidp tmp_mem in
  let result =
    Bindings.C.Functions.js_atod str pnext options.radix flags tmp_mem_ptr
  in
  let next_ptr = Ctypes.( !@ ) pnext in
  if Ctypes.is_null next_ptr then None
  else
    let str_start = Ctypes.coerce Ctypes.string (Ctypes.ptr Ctypes.char) str in
    let offset = Ctypes.ptr_diff str_start next_ptr in
    if offset <= 0 then None
    else
      let remaining =
        Stdlib.String.sub str offset (Stdlib.String.length str - offset)
      in
      Some (result, remaining)
