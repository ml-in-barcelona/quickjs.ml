(* JS_DTOA flags from dtoa.h *)
let js_dtoa_format_free = 0
let js_dtoa_format_fixed = 1
let js_dtoa_format_frac = 2
let _js_dtoa_format_mask = 3
let js_dtoa_exp_auto = 0
let js_dtoa_exp_enabled = 1 lsl 2
let js_dtoa_exp_disabled = 2 lsl 2

(* let js_dtoa_exp_mask = 3 lsl 2 *)
let js_dtoa_minus_zero = 1 lsl 4

(* JS_ATOD flags from dtoa.h *)
let js_atod_int_only = 1 lsl 0
let js_atod_accept_bin_oct = 1 lsl 1
let js_atod_accept_legacy_octal = 1 lsl 2
let js_atod_accept_underscores = 1 lsl 3

(* Maximum digits supported *)
let max_digits = 101

(* Maximum buffer size for integer conversions *)
let max_int_buf_size = 65 (* 64 bits in binary + sign *)

module Dtoa = struct
  type format = Free | Fixed of int | Fractional of int
  type exponent = Auto | Always | Never

  type options = {
    format : format;
    exponent : exponent;
    radix : int;
    show_minus_zero : bool;
  }

  let default_options =
    { format = Free; exponent = Auto; radix = 10; show_minus_zero = false }

  let validate_radix radix =
    if radix < 2 || radix > 36 then
      invalid_arg
        (Printf.sprintf "Dtoa.to_string: radix must be between 2 and 36, got %d"
           radix)

  let validate_digits n =
    if n < 0 || n > max_digits then
      invalid_arg
        (Printf.sprintf
           "Dtoa.to_string: n_digits must be between 0 and %d, got %d"
           max_digits n)

  let options_to_flags options =
    let format_flag =
      match options.format with
      | Free -> js_dtoa_format_free
      | Fixed _ -> js_dtoa_format_fixed
      | Fractional _ -> js_dtoa_format_frac
    in
    let exp_flag =
      match options.exponent with
      | Auto -> js_dtoa_exp_auto
      | Always -> js_dtoa_exp_enabled
      | Never -> js_dtoa_exp_disabled
    in
    let minus_zero_flag =
      if options.show_minus_zero then js_dtoa_minus_zero else 0
    in
    format_flag lor exp_flag lor minus_zero_flag

  let n_digits_from_format format =
    match format with Free -> 0 | Fixed n -> n | Fractional n -> n

  let to_string ?(options = default_options) d =
    validate_radix options.radix;
    let n_digits = n_digits_from_format options.format in
    validate_digits n_digits;
    let flags = options_to_flags options in
    (* Get max buffer size *)
    let max_len =
      Bindings.C.Functions.js_dtoa_max_len d options.radix n_digits flags
    in
    (* Allocate buffer *)
    let buf = Ctypes.allocate_n Ctypes.char ~count:(max_len + 1) in
    (* Allocate temp memory (JSDTOATempMem is 37 * 8 = 296 bytes) *)
    let tmp_mem = Ctypes.allocate_n Ctypes.uint64_t ~count:37 in
    let tmp_mem_ptr = Ctypes.to_voidp tmp_mem in
    (* Call js_dtoa *)
    let actual_len =
      Bindings.C.Functions.js_dtoa buf d options.radix n_digits flags
        tmp_mem_ptr
    in
    (* Extract string *)
    Ctypes.string_from_ptr buf ~length:actual_len

  let to_fixed n d =
    validate_digits n;
    to_string
      ~options:
        {
          format = Fractional n;
          exponent = Never;
          radix = 10;
          show_minus_zero = false;
        }
      d

  let to_precision n d =
    if n < 1 || n > max_digits then
      invalid_arg
        (Printf.sprintf
           "Dtoa.to_precision: precision must be between 1 and %d, got %d"
           max_digits n);
    to_string
      ~options:
        {
          format = Fixed n;
          exponent = Auto;
          radix = 10;
          show_minus_zero = false;
        }
      d

  let to_exponential n d =
    validate_digits n;
    (* JS toExponential(n) uses n+1 significant digits to get n fractional digits
       in exponential notation (e.g., toExponential(2) gives "1.23e+2") *)
    to_string
      ~options:
        {
          format = Fixed (n + 1);
          exponent = Always;
          radix = 10;
          show_minus_zero = false;
        }
      d

  let to_radix radix d =
    validate_radix radix;
    to_string
      ~options:
        { format = Free; exponent = Auto; radix; show_minus_zero = false }
      d
end

module Atod = struct
  type options = {
    radix : int;
    int_only : bool;
    accept_bin_oct : bool;
    accept_legacy_octal : bool;
    accept_underscores : bool;
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
           "Atod.parse: radix must be 0 (auto) or between 2 and 36, got %d"
           radix)

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
      let str_start =
        Ctypes.coerce Ctypes.string (Ctypes.ptr Ctypes.char) str
      in
      let offset = Ctypes.ptr_diff str_start next_ptr in
      if offset <= 0 then None
      else
        let remaining = String.sub str offset (String.length str - offset) in
        Some (result, remaining)
end

module IntToString = struct
  let of_int32 n =
    let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
    let len = Bindings.C.Functions.i32toa buf n in
    Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

  let of_uint32 n =
    let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
    let len = Bindings.C.Functions.u32toa buf n in
    Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

  let of_int64 n =
    let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
    let len = Bindings.C.Functions.i64toa buf n in
    Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

  let of_uint64 n =
    let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
    let len = Bindings.C.Functions.u64toa buf n in
    Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

  let of_int n = of_int64 (Int64.of_int n)

  let of_int32_radix ~radix n =
    if radix < 2 || radix > 36 then
      invalid_arg
        (Printf.sprintf
           "IntToString.of_int32_radix: radix must be between 2 and 36, got %d"
           radix);
    let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
    let len = Bindings.C.Functions.i64toa_radix buf (Int64.of_int32 n) radix in
    Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

  let of_int64_radix ~radix n =
    if radix < 2 || radix > 36 then
      invalid_arg
        (Printf.sprintf
           "IntToString.of_int64_radix: radix must be between 2 and 36, got %d"
           radix);
    let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
    let len = Bindings.C.Functions.i64toa_radix buf n radix in
    Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

  let of_int_radix ~radix n = of_int64_radix ~radix (Int64.of_int n)
end
