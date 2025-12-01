(** JavaScript Global object functions

    This module contains global functions from the JavaScript specification like
    parseFloat and parseInt. *)

type parse_options = {
  radix : int;  (** 0 = auto-detect, 2-36 = fixed radix *)
  int_only : bool;  (** Only accept integers (no dot, no exponent) *)
  accept_bin_oct : bool;  (** Accept 0b and 0o prefixes when radix=0 *)
  accept_legacy_octal : bool;  (** Accept 0777 style (Annex B) when radix=0 *)
  accept_underscores : bool;  (** Accept 1_000_000 as digit separator *)
}
(** Parsing options for string-to-number conversion *)

val default_parse_options : parse_options
(** Default options: radix 10, no special parsing *)

val js_parse_options : parse_options
(** JavaScript-compatible options: auto-detect radix, accept 0b/0o prefixes *)

val parse_float : ?options:parse_options -> string -> float option
(** [parse_float ?options str] parses string [str] as a floating-point number.
    Returns [None] if parsing fails. Equivalent to JavaScript's parseFloat().
    @raise Invalid_argument if radix is invalid *)

val parse_float_partial :
  ?options:parse_options -> string -> (float * string) option
(** [parse_float_partial ?options str] parses the beginning of string [str] as a
    floating-point number, returning the parsed value and remaining string.
    Returns [None] if no valid number is found at the start.
    @raise Invalid_argument if radix is invalid *)

val parse_int : ?radix:int -> string -> int option
(** [parse_int ?radix str] parses string [str] as an integer. [radix] specifies
    the base (2-36), or 0 for auto-detect. Default is 10. Returns [None] if
    parsing fails or radix is invalid. Equivalent to JavaScript's parseInt(). *)
