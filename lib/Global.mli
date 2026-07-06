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
(** Default options: radix 10, no special parsing. This matches JavaScript's
    parseFloat, which never interprets radix prefixes. *)

val js_number_options : parse_options
(** Options matching JavaScript's [Number()] literal syntax: auto-detect radix
    and accept 0x/0b/0o prefixes. Note that this is {b not} how JavaScript's
    parseFloat behaves (parseFloat("0xff") is 0); use {!default_parse_options}
    for parseFloat semantics. *)

val parse_float : ?options:parse_options -> string -> float option
(** [parse_float ?options str] parses the longest numeric prefix of [str] as a
    floating-point number, after skipping leading JavaScript whitespace. Returns
    [None] when no numeric prefix exists. Equivalent to JavaScript's
    parseFloat(), with [None] playing the role of NaN: in particular
    [parse_float "NaN"] is [None], since "NaN" is not part of parseFloat's
    grammar and JavaScript returns NaN for it exactly like for any other
    non-numeric input.

    @raise Invalid_argument if [options.radix] is invalid *)

val parse_float_partial :
  ?options:parse_options -> string -> (float * string) option
(** [parse_float_partial ?options str] parses the beginning of string [str]
    (after skipping leading JavaScript whitespace) as a floating-point number,
    returning the parsed value and the remaining unparsed suffix. Returns [None]
    if no valid number is found at the start.

    Example: [parse_float_partial "3.14abc"] is [Some (3.14, "abc")].

    @raise Invalid_argument if [options.radix] is invalid *)

val parse_int : ?radix:int -> string -> int option
(** [parse_int ?radix str] parses string [str] as an integer, after skipping
    leading JavaScript whitespace. [radix] specifies the base (2-36), or 0 for
    auto-detect; the default is 0, matching JavaScript's parseInt (base 10
    unless the input has a [0x]/[0X] prefix). A [0x] prefix is also accepted
    when [radix] is explicitly 16.

    Returns [None] when parsing fails, the radix is invalid, or the value does
    not fit in an OCaml [int] (JavaScript would return a lossy float in that
    case; this API refuses instead of silently corrupting the value — use
    {!parse_int_float} for JavaScript's exact behavior). Values above 2^53 that
    do fit in [int] follow JavaScript's float precision. *)

val parse_int_float : ?radix:int -> string -> float option
(** [parse_int_float ?radix str] is {!parse_int} returning the value as a float
    — JavaScript's actual number type for parseInt. Values beyond OCaml's [int]
    range (or infinite ones) are returned as floats with JavaScript's precision
    instead of being refused, so [parse_int_float "99999999999999999999999999"]
    is [Some 1e26], exactly like [parseInt] in a browser. *)
