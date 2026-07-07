(** Low-level bindings to QuickJS's libunicode - Unicode Character Utilities

    Unicode character classification and case conversion. These are raw C
    bindings; for a higher-level API, use [Quickjs.Unicode]. *)

(** {2 Character Classification} *)

(** Check if character has uppercase/lowercase variants (Cased property) *)
let is_cased cp = Quickjs_bindings.C.Functions.lre_is_cased cp

(** Check if character is ignored during case mapping (Case_Ignorable) *)
let is_case_ignorable cp = Quickjs_bindings.C.Functions.lre_is_case_ignorable cp

(** Check if character can start an identifier (ID_Start) *)
let is_id_start cp = Quickjs_bindings.C.Functions.lre_is_id_start cp

(** Check if character can continue an identifier (ID_Continue) *)
let is_id_continue cp = Quickjs_bindings.C.Functions.lre_is_id_continue cp

(** Check if character is whitespace per ECMA-262 (WhiteSpace or
    LineTerminator). This is the exact predicate QuickJS uses for regexp \s,
    String.prototype.trim, and the whitespace skipped by parseInt/parseFloat. *)
let is_space cp = Quickjs_bindings.C.Functions.lre_is_space cp

(** {2 Case Conversion} *)

(** Convert character case. conv_type: 0 = uppercase, 1 = lowercase, 2 = case
    folding Returns number of output codepoints (1-3) *)
let case_conv res cp conv_type =
  Quickjs_bindings.C.Functions.lre_case_conv res cp conv_type

(** Canonicalize character for case-insensitive regex matching. is_unicode: true
    = full Unicode folding, false = ASCII only *)
let canonicalize cp is_unicode =
  Quickjs_bindings.C.Functions.lre_canonicalize cp is_unicode

(** {2 Normalization} *)

(** Normalize Unicode string. n_type: 0 = NFC, 1 = NFD, 2 = NFKC, 3 = NFKD
    Returns length of output, or -1 on error *)
let normalize src len n_type dst =
  Quickjs_bindings.C.Functions.unicode_normalize_shim src len n_type dst

(** Free buffer allocated by [normalize] *)
let normalize_free ptr = Quickjs_bindings.C.Functions.unicode_normalize_free ptr

(** {2 Character Ranges} *)

(** Look up a Unicode character range table by name. kind: 0 = Script, 1 =
    Script_Extensions, 2 = General_Category, 3 = binary property. On success
    returns the number of points (always even) with the buffer stored in [dst];
    each pair [points.(2i), points.(2i+1)] encodes a half-open interval. Release
    the buffer with [char_range_free]. Returns -1 on memory error, -2 when the
    name is unknown. *)
let char_range kind name dst =
  Quickjs_bindings.C.Functions.unicode_char_range_shim kind name dst

(** Free buffer returned by [char_range] *)
let char_range_free ptr =
  Quickjs_bindings.C.Functions.unicode_char_range_free ptr
