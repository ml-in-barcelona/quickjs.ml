(** JavaScript String built-in object

    This module mirrors the JavaScript String API with prototype methods for
    string manipulation. *)

(** Unicode normalization forms *)
type normalization = NFC | NFD | NFKC | NFKD

module Prototype : sig
  (** String.prototype methods *)

  val to_lower_case : string -> string
  (** [to_lower_case s] returns a new string with all characters converted to
      lowercase. Equivalent to JavaScript's String.prototype.toLowerCase(). *)

  val to_upper_case : string -> string
  (** [to_upper_case s] returns a new string with all characters converted to
      uppercase. Equivalent to JavaScript's String.prototype.toUpperCase(). *)

  val normalize : normalization -> string -> string option
  (** [normalize form s] returns the Unicode Normalization Form of string [s].
      Returns [None] if normalization fails. Equivalent to JavaScript's
      String.prototype.normalize(). *)
end

val lowercase_char : Uchar.t -> Uchar.t list
(** [lowercase_char c] converts a single character to lowercase. May return
    multiple characters for special cases. *)

val uppercase_char : Uchar.t -> Uchar.t list
(** [uppercase_char c] converts a single character to uppercase. May return
    multiple characters (e.g., ß -> SS). *)
