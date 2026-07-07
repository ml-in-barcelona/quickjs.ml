(** Unicode utilities from QuickJS's libunicode

    This module provides Unicode character classification, case conversion, and
    normalization functions. It uses the same battle-tested Unicode tables as
    QuickJS's ES2023-compliant JavaScript engine. *)

(** {1 Normalization} *)

(** Unicode normalization forms *)
type normalization =
  | NFC  (** Canonical Decomposition, followed by Canonical Composition *)
  | NFD  (** Canonical Decomposition *)
  | NFKC  (** Compatibility Decomposition, followed by Canonical Composition *)
  | NFKD  (** Compatibility Decomposition *)

val normalize : normalization -> string -> string option
(** [normalize form str] normalizes a UTF-8 string to the specified form.
    Returns [None] on memory allocation failure or invalid input.

    Example:
    {[
      let composed = Unicode.normalize NFC "cafe\xcc\x81" in
      (* composed = Some "café" (e + combining accent composed to é) *)
      let decomposed = Unicode.normalize NFD "café" in
      (* decomposed = Some "cafe\xcc\x81" *)
      ignore (composed, decomposed)
    ]} *)

(** {1 Case Conversion} *)

val lowercase : string -> string
(** [lowercase str] converts a UTF-8 string to lowercase. Handles Unicode
    characters like "ÉCOLE" → "école". *)

val uppercase : string -> string
(** [uppercase str] converts a UTF-8 string to uppercase. Handles special cases
    like "ß" → "SS". *)

val fold_case : string -> string
(** [fold_case str] applies Unicode full case folding to a UTF-8 string, e.g.
    "Straße" → "strasse" and "ΣΤΙΓΜΑΣ" → "στιγμασ". Case folding is
    context-independent (no Final_Sigma rule) and locale-independent; two
    strings are caseless-equal when their foldings are equal. This is the same
    folding QuickJS uses for case-insensitive matching. *)

(** {1 Single Character Operations} *)

val lowercase_char : Uchar.t -> Uchar.t list
(** [lowercase_char c] returns the lowercase form of a code point. Returns a
    list because some characters expand (though lowercase rarely does). *)

val uppercase_char : Uchar.t -> Uchar.t list
(** [uppercase_char c] returns the uppercase form of a code point. Returns a
    list because some characters expand, e.g., 'ß' → ['S'; 'S']. *)

val fold_case_char : Uchar.t -> Uchar.t list
(** [fold_case_char c] returns the Unicode full case folding of a code point.
    Returns a list because some characters expand, e.g., 'ß' → ['s'; 's']. *)

(** {1 Character Classification} *)

val is_cased : Uchar.t -> bool
(** [is_cased c] returns [true] if the character has uppercase/lowercase forms.
    Examples: 'a', 'A', 'é' are cased; '1', '!' are not. *)

val is_case_ignorable : Uchar.t -> bool
(** [is_case_ignorable c] returns [true] if the character is ignored during case
    mapping operations (e.g., combining marks). *)

val is_id_start : Uchar.t -> bool
(** [is_id_start c] returns [true] if the character can start a
    JavaScript/Unicode identifier (letters, $, _). *)

val is_id_continue : Uchar.t -> bool
(** [is_id_continue c] returns [true] if the character can continue a
    JavaScript/Unicode identifier (letters, digits, $, _, combining marks). *)

val is_whitespace : Uchar.t -> bool
(** [is_whitespace c] returns [true] if the character is Unicode whitespace.
    Includes ASCII space, tab, newline, and Unicode spaces like U+00A0 (NBSP).
*)

(** {1 Character Sets}

    Lookups into the Unicode property tables QuickJS uses for regexp [\p{...}]
    escapes: Script, Script_Extensions, General_Category and the binary
    properties. Names are the canonical Unicode names and their aliases, matched
    case-sensitively, exactly like JavaScript's [\p{...}]. *)

(** A set of Unicode code points, stored as sorted disjoint ranges. *)
module CharSet : sig
  type t

  val mem : Uchar.t -> t -> bool
  (** [mem c set] tests whether code point [c] belongs to [set]. *)

  val ranges : t -> (int * int) array
  (** [ranges set] returns the set as inclusive code point ranges
      [(first, last)], sorted by increasing value and disjoint. Ranges are plain
      ints rather than [Uchar.t] because some sets contain surrogate code points
      (e.g. General_Category [Cs]). *)
end

val script : ?extensions:bool -> string -> CharSet.t option
(** [script ?extensions name] returns the set of code points whose Script
    property is [name], or [None] if [name] is not a known script. Accepts long
    and short names ("Greek" or "Grek"). With [~extensions:true] the
    Script_Extensions property is used instead, like JavaScript's
    [\p{Script_Extensions=...}].

    Example:
    {[
      let greek = Option.get (Unicode.script "Greek") in
      assert (Unicode.CharSet.mem (Uchar.of_int 0x03B1) greek)
      (* α *)
    ]} *)

val general_category : string -> CharSet.t option
(** [general_category name] returns the set of code points whose
    General_Category is [name], or [None] if [name] is not a known category.
    Accepts short and long names ("Lu" or "Uppercase_Letter"), including the
    grouped categories ("L", "Letter", ...), like JavaScript's [\p{L}]. *)

val binary_property : string -> CharSet.t option
(** [binary_property name] returns the set of code points with the given binary
    property, or [None] if [name] is not a known property. Accepts names like
    "Alphabetic", "White_Space", "Emoji" and their aliases, like JavaScript's
    [\p{Alphabetic}]. *)

(** {1 Regex Support} *)

val canonicalize : ?unicode:bool -> Uchar.t -> Uchar.t
(** [canonicalize ?unicode c] returns the canonical form of a character for
    case-insensitive regex matching.
    - [unicode]: if [true] (default), use full Unicode case folding; if [false],
      only ASCII case folding. *)
