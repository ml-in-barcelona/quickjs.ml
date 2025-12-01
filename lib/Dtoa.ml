(** JavaScript-compatible double-to-string conversion

    This module provides bindings to QuickJS's dtoa library for double-to-string
    conversion with JavaScript-compatible semantics. *)

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

(* Maximum digits supported *)
let max_digits = 101

(** Number of digits to use *)
type format =
  | Free  (** Use minimum digits needed for exact round-trip *)
  | Fixed of int  (** Use n significant digits (1-101) *)
  | Fractional of int  (** Use n fractional digits (0-101) *)

(** Exponential notation control *)
type exponent =
  | Auto  (** Use exponential when appropriate *)
  | Always  (** Always use exponential notation *)
  | Never  (** Never use exponential notation *)

type options = {
  format : format;  (** Digit format *)
  exponent : exponent;  (** Exponential notation *)
  radix : int;  (** Base 2-36, default 10 *)
  show_minus_zero : bool;  (** Show "-0" for negative zero *)
}
(** Full options for formatting *)

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
         "Dtoa.to_string: n_digits must be between 0 and %d, got %d" max_digits
         n)

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
    Bindings.C.Functions.js_dtoa buf d options.radix n_digits flags tmp_mem_ptr
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
    ~options:{ format = Free; exponent = Auto; radix; show_minus_zero = false }
    d
