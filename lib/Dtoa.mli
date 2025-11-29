(** JavaScript-compatible number formatting

    This module provides bindings to QuickJS's dtoa library for double-to-string
    and string-to-double conversion with JavaScript-compatible semantics. *)

(** {1 Double to String Conversion} *)

module Dtoa : sig
  (** Formatting options for double-to-string conversion *)

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

  val default_options : options
  (** Default options: Free format, Auto exponent, radix 10, no minus zero *)

  val to_string : ?options:options -> float -> string
  (** [to_string ?options d] converts float [d] to string with JS-compatible
      formatting.

      @raise Invalid_argument if radix is not in range 2-36 or n_digits > 101 *)

  val to_fixed : int -> float -> string
  (** [to_fixed n d] formats [d] with [n] fractional digits (like JS toFixed()).

      @raise Invalid_argument if n is not in range 0-101 *)

  val to_precision : int -> float -> string
  (** [to_precision n d] formats [d] with [n] significant digits (like JS
      toPrecision()).

      @raise Invalid_argument if n is not in range 1-101 *)

  val to_exponential : int -> float -> string
  (** [to_exponential n d] formats [d] in exponential notation with [n]
      fractional digits (like JS toExponential()).

      @raise Invalid_argument if n is not in range 0-101 *)

  val to_radix : int -> float -> string
  (** [to_radix radix d] formats [d] in the given radix (like JS
      toString(radix)).

      @raise Invalid_argument if radix is not in range 2-36 *)
end

(** {1 String to Double Conversion} *)

module Atod : sig
  (** Parsing options for string-to-double conversion *)

  type options = {
    radix : int;  (** 0 = auto-detect, 2-36 = fixed radix *)
    int_only : bool;  (** Only accept integers (no dot, no exponent) *)
    accept_bin_oct : bool;  (** Accept 0b and 0o prefixes when radix=0 *)
    accept_legacy_octal : bool;  (** Accept 0777 style (Annex B) when radix=0 *)
    accept_underscores : bool;  (** Accept 1_000_000 as digit separator *)
  }

  val default_options : options
  (** Default: radix 10, no special options *)

  val js_options : options
  (** Match JavaScript parseFloat behavior: auto-detect radix, accept 0b/0o *)

  val parse : ?options:options -> string -> float option
  (** [parse ?options str] parses [str] to a float. Returns [None] if the string
      cannot be parsed.

      @raise Invalid_argument if radix is not 0 or in range 2-36 *)

  val parse_partial : ?options:options -> string -> (float * string) option
  (** [parse_partial ?options str] parses [str] and returns the parsed value
      along with the unparsed remainder. Returns [None] if no valid number
      prefix found.

      @raise Invalid_argument if radix is not 0 or in range 2-36 *)
end

(** {1 Fast Integer to String Conversion} *)

module IntToString : sig
  (** Fast integer to string conversion using optimized C implementation *)

  val of_int32 : int32 -> string
  (** Convert int32 to decimal string *)

  val of_uint32 : Unsigned.uint32 -> string
  (** Convert uint32 to decimal string *)

  val of_int64 : int64 -> string
  (** Convert int64 to decimal string *)

  val of_uint64 : Unsigned.uint64 -> string
  (** Convert uint64 to decimal string *)

  val of_int : int -> string
  (** Convert int to decimal string *)

  val of_int32_radix : radix:int -> int32 -> string
  (** Convert int32 to string in given radix (2-36).

      @raise Invalid_argument if radix is not in range 2-36 *)

  val of_int64_radix : radix:int -> int64 -> string
  (** Convert int64 to string in given radix (2-36).

      @raise Invalid_argument if radix is not in range 2-36 *)

  val of_int_radix : radix:int -> int -> string
  (** Convert int to string in given radix (2-36).

      @raise Invalid_argument if radix is not in range 2-36 *)
end
