(** JavaScript-compatible string-to-double conversion

    This module provides bindings to QuickJS's dtoa library for string-to-double
    conversion with JavaScript-compatible semantics. *)

(** {1 Parsing Options} *)

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

(** {1 Parsing Functions} *)

val parse : ?options:options -> string -> float option
(** [parse ?options str] parses [str] to a float. Returns [None] if the string
    cannot be parsed.

    @raise Invalid_argument if radix is not 0 or in range 2-36 *)

val parse_partial : ?options:options -> string -> (float * string) option
(** [parse_partial ?options str] parses [str] and returns the parsed value along
    with the unparsed remainder. Returns [None] if no valid number prefix found.

    @raise Invalid_argument if radix is not 0 or in range 2-36 *)

