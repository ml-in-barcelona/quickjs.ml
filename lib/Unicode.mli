(** Unicode utilities from QuickJS's libunicode

    This module provides Unicode character classification, case conversion,
    and normalization functions. It uses the same battle-tested Unicode
    tables as QuickJS's ES2023-compliant JavaScript engine. *)

(** {1 Normalization} *)

(** Unicode normalization forms *)
type normalization =
  | NFC   (** Canonical Decomposition, followed by Canonical Composition *)
  | NFD   (** Canonical Decomposition *)
  | NFKC  (** Compatibility Decomposition, followed by Canonical Composition *)
  | NFKD  (** Compatibility Decomposition *)

val normalize : normalization -> string -> string option
(** [normalize form str] normalizes a UTF-8 string to the specified form.
    Returns [None] on memory allocation failure or invalid input.

    Example:
    {[
      normalize NFC "café"  (* composed form *)
      normalize NFD "café"  (* decomposed form *)
    ]} *)

(** {1 Case Conversion} *)

val lowercase : string -> string
(** [lowercase str] converts a UTF-8 string to lowercase.
    Handles Unicode characters like "ÉCOLE" → "école". *)

val uppercase : string -> string
(** [uppercase str] converts a UTF-8 string to uppercase.
    Handles special cases like "ß" → "SS". *)

(** {1 Single Character Operations} *)

val lowercase_char : Uchar.t -> Uchar.t list
(** [lowercase_char c] returns the lowercase form of a code point.
    Returns a list because some characters expand (though lowercase rarely does). *)

val uppercase_char : Uchar.t -> Uchar.t list
(** [uppercase_char c] returns the uppercase form of a code point.
    Returns a list because some characters expand, e.g., 'ß' → ['S'; 'S']. *)

(** {1 Character Classification} *)

val is_cased : Uchar.t -> bool
(** [is_cased c] returns [true] if the character has uppercase/lowercase forms.
    Examples: 'a', 'A', 'é' are cased; '1', '!' are not. *)

val is_case_ignorable : Uchar.t -> bool
(** [is_case_ignorable c] returns [true] if the character is ignored
    during case mapping operations (e.g., combining marks). *)

val is_id_start : Uchar.t -> bool
(** [is_id_start c] returns [true] if the character can start a
    JavaScript/Unicode identifier (letters, $, _). *)

val is_id_continue : Uchar.t -> bool
(** [is_id_continue c] returns [true] if the character can continue a
    JavaScript/Unicode identifier (letters, digits, $, _, combining marks). *)

val is_whitespace : Uchar.t -> bool
(** [is_whitespace c] returns [true] if the character is Unicode whitespace.
    Includes ASCII space, tab, newline, and Unicode spaces like U+00A0 (NBSP). *)

(** {1 Regex Support} *)

val canonicalize : ?unicode:bool -> Uchar.t -> Uchar.t
(** [canonicalize ?unicode c] returns the canonical form of a character
    for case-insensitive regex matching.
    - [unicode]: if [true] (default), use full Unicode case folding;
      if [false], only ASCII case folding. *)

