(** Low-level bindings to QuickJS's libunicode - Unicode Character Utilities

    Unicode character classification and case conversion. These are raw C
    bindings; for a higher-level API, use [Quickjs.Unicode]. *)

(** {2 Character Classification} *)

(** Check if character has uppercase/lowercase variants (Cased property) *)
let is_cased cp = Bindings.C.Functions.lre_is_cased cp

(** Check if character is ignored during case mapping (Case_Ignorable) *)
let is_case_ignorable cp = Bindings.C.Functions.lre_is_case_ignorable cp

(** Check if character can start an identifier (ID_Start) *)
let is_id_start cp = Bindings.C.Functions.lre_is_id_start cp

(** Check if character can continue an identifier (ID_Continue) *)
let is_id_continue cp = Bindings.C.Functions.lre_is_id_continue cp

(** Check if non-ASCII character is whitespace (for codepoints >= 256) *)
let is_space_non_ascii cp = Bindings.C.Functions.lre_is_space_non_ascii cp

(** {2 Case Conversion} *)

(** Convert character case. conv_type: 0 = uppercase, 1 = lowercase, 2 = case
    folding Returns number of output codepoints (1-3) *)
let case_conv res cp conv_type =
  Bindings.C.Functions.lre_case_conv res cp conv_type

(** Canonicalize character for case-insensitive regex matching. is_unicode: 1 =
    full Unicode folding, 0 = ASCII only *)
let canonicalize cp is_unicode =
  Bindings.C.Functions.lre_canonicalize cp is_unicode

(** {2 Normalization} *)

(** Normalize Unicode string. n_type: 0 = NFC, 1 = NFD, 2 = NFKC, 3 = NFKD
    Returns length of output, or -1 on error *)
let normalize src len n_type dst =
  Bindings.C.Functions.unicode_normalize_shim src len n_type dst

(** Free buffer allocated by [normalize] *)
let normalize_free ptr = Bindings.C.Functions.unicode_normalize_free ptr
