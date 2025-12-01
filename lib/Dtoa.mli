(** JavaScript-compatible double-to-string conversion

    This module provides bindings to QuickJS's dtoa library for double-to-string
    conversion with JavaScript-compatible semantics. *)

(** {1 Formatting Options} *)

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

(** {1 Conversion Functions} *)

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
(** [to_exponential n d] formats [d] in exponential notation with [n] fractional
    digits (like JS toExponential()).

    @raise Invalid_argument if n is not in range 0-101 *)

val to_radix : int -> float -> string
(** [to_radix radix d] formats [d] in the given radix (like JS toString(radix)).

    @raise Invalid_argument if radix is not in range 2-36 *)
