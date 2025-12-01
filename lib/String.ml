(** JavaScript String built-in object

    This module mirrors the JavaScript String API with prototype methods for
    string manipulation. *)

(* Re-export normalization type from Unicode *)
type normalization = Unicode.normalization = NFC | NFD | NFKC | NFKD

module Prototype = struct
  (** String.prototype methods *)

  (** [to_lower_case s] returns a new string with all characters converted to
      lowercase. Equivalent to JavaScript's String.prototype.toLowerCase(). *)
  let to_lower_case = Unicode.lowercase

  (** [to_upper_case s] returns a new string with all characters converted to
      uppercase. Equivalent to JavaScript's String.prototype.toUpperCase(). *)
  let to_upper_case = Unicode.uppercase

  (** [normalize form s] returns the Unicode Normalization Form of string [s].
      Equivalent to JavaScript's String.prototype.normalize(). *)
  let normalize = Unicode.normalize
end

(* Character-level case conversion *)

(** [lowercase_char c] converts a single character to lowercase. May return
    multiple characters for special cases. *)
let lowercase_char = Unicode.lowercase_char

(** [uppercase_char c] converts a single character to uppercase. May return
    multiple characters (e.g., ß -> SS). *)
let uppercase_char = Unicode.uppercase_char
