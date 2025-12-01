(** JavaScript Number built-in object

    This module mirrors the JavaScript Number API with prototype methods for
    number-to-string conversion. *)

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
  format : format;
  exponent : exponent;
  radix : int;
  show_minus_zero : bool;
}
(** Full options for formatting *)

val default_options : options
(** Default options: Free format, Auto exponent, radix 10, no minus zero *)

module Prototype : sig
  (** Number.prototype methods *)

  val to_string : ?options:options -> float -> string
  (** [to_string ?options n] converts number [n] to string with JS-compatible
      formatting. Equivalent to JavaScript's Number.prototype.toString().
      @raise Invalid_argument if radix is not in range 2-36 *)

  val to_fixed : int -> float -> string
  (** [to_fixed digits n] formats [n] with [digits] fractional digits.
      Equivalent to JavaScript's Number.prototype.toFixed().
      @raise Invalid_argument if digits is not in range 0-101 *)

  val to_precision : int -> float -> string
  (** [to_precision digits n] formats [n] with [digits] significant digits.
      Equivalent to JavaScript's Number.prototype.toPrecision().
      @raise Invalid_argument if digits is not in range 1-101 *)

  val to_exponential : int -> float -> string
  (** [to_exponential digits n] formats [n] in exponential notation. Equivalent
      to JavaScript's Number.prototype.toExponential().
      @raise Invalid_argument if digits is not in range 0-101 *)

  val to_radix : int -> float -> string
  (** [to_radix radix n] formats [n] in the given radix (2-36). Equivalent to
      JavaScript's Number.prototype.toString(radix).
      @raise Invalid_argument if radix is not in range 2-36 *)
end

(** {1 Integer-to-string conversions} *)

val of_int : int -> string
(** [of_int n] converts integer [n] to string. *)

val of_int32 : int32 -> string
(** [of_int32 n] converts 32-bit integer [n] to string. *)

val of_int64 : int64 -> string
(** [of_int64 n] converts 64-bit integer [n] to string. *)

val of_uint32 : Unsigned.UInt32.t -> string
(** [of_uint32 n] converts unsigned 32-bit integer [n] to string. *)

val of_uint64 : Unsigned.UInt64.t -> string
(** [of_uint64 n] converts unsigned 64-bit integer [n] to string. *)

val of_int_radix : radix:int -> int -> string
(** [of_int_radix ~radix n] converts integer [n] to string in given radix.
    @raise Invalid_argument if radix is not in range 2-36 *)

val of_int32_radix : radix:int -> int32 -> string
(** [of_int32_radix ~radix n] converts 32-bit integer [n] to string in given
    radix.
    @raise Invalid_argument if radix is not in range 2-36 *)

val of_int64_radix : radix:int -> int64 -> string
(** [of_int64_radix ~radix n] converts 64-bit integer [n] to string in given
    radix.
    @raise Invalid_argument if radix is not in range 2-36 *)
