(** JavaScript String built-in object

    This module mirrors the JavaScript String API with prototype methods for
    string manipulation. All methods use UTF-16 semantics for indices.

    {2 JavaScript vs OCaml Semantics}

    This library follows JavaScript semantics in most cases to ensure
    compatibility with the ECMA-262 specification:

    - {b Indices}: All string indices are UTF-16 code unit positions, not byte
      positions. A string containing an emoji has length 2 (surrogate pair).
      Indices returned by the RegExp-based methods use the same unit.

    - {b Not-found values}: Methods like [index_of] return [-1] when not found
      (JavaScript convention), not [option] (OCaml convention). This matches
      JavaScript's String.prototype.indexOf().

    - {b Character access}: Methods like [char_code_at] return [int option]
      (OCaml convention) for bounds checking, since OCaml does not have
      JavaScript's implicit NaN coercion.

    - {b Negative indices}: Methods like [slice] support negative indices
      counting from the end, matching JavaScript behavior.

    - {b Invalid patterns}: RegExp-based methods raise [Invalid_argument] on
      invalid patterns, mirroring the SyntaxError JavaScript throws for invalid
      regular expression literals. *)

(** Unicode normalization forms *)
type normalization = NFC | NFD | NFKC | NFKD

val is_valid_utf8 : string -> bool
(** [is_valid_utf8 s] returns [true] if [s] contains only valid UTF-8 byte
    sequences. Use this for strict validation before processing untrusted input.

    Note: All functions in this module handle invalid UTF-8 gracefully by
    replacing malformed sequences with U+FFFD (replacement character). *)

val utf16_index_of_byte : string -> int -> int
(** [utf16_index_of_byte s i] converts a UTF-8 byte offset in [s] into the
    corresponding UTF-16 code unit index (the unit used by every index in this
    module and in {!RegExp}). Offsets falling inside a multi-byte sequence map
    to the index of that character; out-of-range offsets are clamped. Useful at
    the boundary with byte-oriented code. *)

val byte_index_of_utf16 : string -> int -> int
(** [byte_index_of_utf16 s i] converts a UTF-16 code unit index into the
    corresponding UTF-8 byte offset in [s]. Indices falling inside a surrogate
    pair map to the byte offset of that character; out-of-range indices are
    clamped. Inverse of {!utf16_index_of_byte}. *)

(** {1 Static methods} *)

val from_char_code : int array -> string
(** [from_char_code codes] builds a string from UTF-16 code units. Every value
    is coerced with ToUint16 (kept modulo 2{^16}, so
    [from_char_code [| 0x10041 |]] is ["A"]) and adjacent high/low surrogate
    values combine into one code point: [from_char_code [| 0xD83D; 0xDE00 |]]
    is ["😀"]. Equivalent to JavaScript's String.fromCharCode().

    Unpaired surrogates become U+FFFD, since UTF-8 cannot represent them
    (JavaScript builds a lone-surrogate string instead). *)

val from_code_point : int array -> string
(** [from_code_point code_points] builds a string from Unicode code points:
    [from_code_point [| 0x1F600 |]] is ["😀"]. Equivalent to JavaScript's
    String.fromCodePoint().

    Surrogate halves given as separate code points pair up exactly like in
    JavaScript ([from_code_point [| 0xD83D; 0xDE00 |]] is also ["😀"]);
    unpaired surrogates become U+FFFD, since UTF-8 cannot represent them.

    @raise Invalid_argument
      if any value is outside [0]..[0x10FFFF], mirroring the RangeError
      JavaScript throws. *)

module Prototype : sig
  (** String.prototype methods *)

  (** {1 Length} *)

  val length : string -> int
  (** [length s] returns the UTF-16 length of string [s] (number of code units).
      Note: This differs from OCaml's String.length which returns byte count. *)

  (** {1 Case conversion} *)

  val to_lower_case : string -> string
  (** [to_lower_case s] returns a new string with all characters converted to
      lowercase, including context-sensitive mappings such as the Greek final
      sigma. Equivalent to JavaScript's String.prototype.toLowerCase(). *)

  val to_upper_case : string -> string
  (** [to_upper_case s] returns a new string with all characters converted to
      uppercase. Equivalent to JavaScript's String.prototype.toUpperCase(). *)

  val normalize : normalization -> string -> string option
  (** [normalize form s] returns the Unicode Normalization Form of string [s].
      Returns [None] if normalization fails. Equivalent to JavaScript's
      String.prototype.normalize(). *)

  (** {1 Character access} *)

  val char_at : int -> string -> string
  (** [char_at idx s] returns the character at UTF-16 index [idx] as a string.
      Returns empty string if index is out of bounds. Equivalent to JavaScript's
      String.prototype.charAt(). *)

  val char_code_at : int -> string -> int option
  (** [char_code_at idx s] returns the UTF-16 code unit at index [idx]. Returns
      [None] if index is out of bounds. Equivalent to JavaScript's
      String.prototype.charCodeAt(). *)

  val code_point_at : int -> string -> int option
  (** [code_point_at idx s] returns the full Unicode code point at UTF-16 index
      [idx]. For surrogate pairs, returns the full code point when idx points to
      the high surrogate. Returns [None] if index is out of bounds. Equivalent
      to JavaScript's String.prototype.codePointAt(). *)

  (** {1 Substring extraction} *)

  val slice : start:int -> end_:int -> string -> string
  (** [slice ~start ~end_ s] extracts a section of string. Negative indices
      count from the end. Equivalent to JavaScript's String.prototype.slice().
  *)

  val slice_from : int -> string -> string
  (** [slice_from start s] extracts from [start] to end of string. *)

  val substring : start:int -> end_:int -> string -> string
  (** [substring ~start ~end_ s] extracts characters between two indices. Swaps
      arguments if start > end. Negative values treated as 0. Equivalent to
      JavaScript's String.prototype.substring(). *)

  val substring_from : int -> string -> string
  (** [substring_from start s] extracts from [start] to end of string. *)

  val substr : start:int -> length:int -> string -> string
  (** [substr ~start ~length s] extracts [length] characters starting from
      [start]. Negative start counts from end. Legacy method (Annex B).
      Equivalent to JavaScript's String.prototype.substr(). *)

  val substr_from : int -> string -> string
  (** [substr_from start s] extracts from [start] to end of string. *)

  (** {1 Search methods} *)

  val index_of : string -> string -> int
  (** [index_of search s] returns UTF-16 index of first occurrence of [search]
      in [s]. Returns -1 if not found (JavaScript semantics). Equivalent to
      JavaScript's String.prototype.indexOf(). *)

  val index_of_from : string -> int -> string -> int
  (** [index_of_from search pos s] searches starting from position [pos]. *)

  val last_index_of : string -> string -> int
  (** [last_index_of search s] returns UTF-16 index of last occurrence of
      [search] in [s]. Returns -1 if not found. Equivalent to JavaScript's
      String.prototype.lastIndexOf(). *)

  val last_index_of_from : string -> int -> string -> int
  (** [last_index_of_from search pos s] searches backwards starting from
      position [pos]. *)

  val includes : string -> string -> bool
  (** [includes search s] returns true if [s] contains [search]. Equivalent to
      JavaScript's String.prototype.includes(). *)

  val includes_from : string -> int -> string -> bool
  (** [includes_from search pos s] checks from position [pos]. *)

  val starts_with : string -> string -> bool
  (** [starts_with search s] returns true if [s] starts with [search].
      Equivalent to JavaScript's String.prototype.startsWith(). *)

  val starts_with_from : string -> int -> string -> bool
  (** [starts_with_from search pos s] checks if [s] starts with [search] at
      position [pos]. Out-of-range positions are clamped. *)

  val ends_with : string -> string -> bool
  (** [ends_with search s] returns true if [s] ends with [search]. Equivalent to
      JavaScript's String.prototype.endsWith(). *)

  val ends_with_at : string -> int -> string -> bool
  (** [ends_with_at search end_pos s] checks if [s] (up to [end_pos]) ends with
      [search]. *)

  (** {1 Transform methods} *)

  val trim : string -> string
  (** [trim s] removes leading and trailing JavaScript whitespace (the ECMA-262
      WhiteSpace and LineTerminator sets). Equivalent to JavaScript's
      String.prototype.trim(). *)

  val trim_start : string -> string
  (** [trim_start s] removes leading whitespace. Equivalent to JavaScript's
      String.prototype.trimStart(). *)

  val trim_end : string -> string
  (** [trim_end s] removes trailing whitespace. Equivalent to JavaScript's
      String.prototype.trimEnd(). *)

  val pad_start : int -> string -> string
  (** [pad_start target_len s] pads [s] at the start with spaces to reach
      [target_len]. Equivalent to JavaScript's String.prototype.padStart(). *)

  val pad_start_with : int -> string -> string -> string
  (** [pad_start_with target_len fill_str s] pads [s] at the start with
      [fill_str]. *)

  val pad_end : int -> string -> string
  (** [pad_end target_len s] pads [s] at the end with spaces to reach
      [target_len]. Equivalent to JavaScript's String.prototype.padEnd(). *)

  val pad_end_with : int -> string -> string -> string
  (** [pad_end_with target_len fill_str s] pads [s] at the end with [fill_str].
  *)

  val repeat : int -> string -> string
  (** [repeat n s] returns [s] repeated [n] times.
      @raise Invalid_argument
        if [n] is negative. Equivalent to JavaScript's
        String.prototype.repeat(). *)

  (** {1 RegExp-based methods}

      All methods in this section compile their pattern argument and raise
      [Invalid_argument] when it is not a valid regular expression, like the
      SyntaxError JavaScript throws. *)

  val match_ : string -> string -> RegExp.match_result option
  (** [match_ pattern s] returns the first match of [pattern] in [s], with
      captures, named groups and index, or [None] if there is no match.
      Equivalent to JavaScript's String.prototype.match() without the global
      flag.
      @raise Invalid_argument if [pattern] is invalid. *)

  val match_flags : string -> string -> string -> RegExp.match_result option
  (** [match_flags pattern flags s] matches with the given regex flags. The
      ['g'] flag is rejected because a single [match_result] cannot represent
      multiple matches; use {!match_global} or {!match_all} instead.
      @raise Invalid_argument if [pattern] or [flags] is invalid. *)

  val match_global : ?flags:string -> string -> string -> string array
  (** [match_global ?flags pattern s] returns all full matches, like
      JavaScript's String.prototype.match() with the global flag (which returns
      full matches only, without capture groups). Returns [[||]] when there is
      no match (JavaScript returns null). The global flag is added
      automatically; [?flags] can supply extra flags such as ["i"].
      @raise Invalid_argument if [pattern] or [flags] is invalid. *)

  val match_all : ?flags:string -> string -> string -> RegExp.match_result list
  (** [match_all ?flags pattern s] returns all match results, with captures,
      named groups and indices. Equivalent to JavaScript's
      String.prototype.matchAll(). The global flag is added automatically.
      @raise Invalid_argument if [pattern] or [flags] is invalid. *)

  val search : string -> string -> int
  (** [search pattern s] returns UTF-16 index of first match, or -1 if not found
      (JavaScript semantics). Equivalent to JavaScript's
      String.prototype.search().
      @raise Invalid_argument if [pattern] is invalid. *)

  val search_flags : string -> string -> string -> int
  (** [search_flags pattern flags s] searches with specified regex flags.
      @raise Invalid_argument if [pattern] or [flags] is invalid. *)

  val replace : string -> string -> string -> string
  (** [replace search replacement s] replaces first occurrence of [search] with
      [replacement]. Equivalent to JavaScript's String.prototype.replace() with
      string argument. Supports the [$$], [$&], [$`] and [$'] replacement
      patterns; [$`] and [$'] always refer to the original string. *)

  val replace_regex : string -> string -> string -> string
  (** [replace_regex pattern replacement s] replaces the first regex match.
      Capture groups are available as [$1]..[$99] in [replacement]; groups that
      did not participate substitute the empty string.
      @raise Invalid_argument if [pattern] is invalid. *)

  val replace_regex_global : string -> string -> string -> string
  (** [replace_regex_global pattern replacement s] replaces all regex matches.
      @raise Invalid_argument if [pattern] is invalid. *)

  val replace_regex_flags : string -> string -> string -> string -> string
  (** [replace_regex_flags pattern flags replacement s] replaces with specified
      flags.
      @raise Invalid_argument if [pattern] or [flags] is invalid. *)

  val replace_all : string -> string -> string -> string
  (** [replace_all search replacement s] replaces all occurrences. Equivalent to
      JavaScript's String.prototype.replaceAll() with a string pattern. *)

  val replace_all_regex : string -> string -> string -> string
  (** [replace_all_regex pattern replacement s] replaces all regex matches.
      @raise Invalid_argument if [pattern] is invalid. *)

  val split : string -> string -> string array
  (** [split separator s] splits [s] by [separator]. Equivalent to JavaScript's
      String.prototype.split(). *)

  val split_limit : string -> int -> string -> string array
  (** [split_limit separator limit s] splits with maximum [limit] parts. *)

  val split_regex : string -> string -> string array
  (** [split_regex pattern s] splits by regex pattern. Capture groups are
      spliced into the result, as in JavaScript; groups that did not participate
      contribute [""].
      @raise Invalid_argument if [pattern] is invalid. *)
end

val lowercase_char : Uchar.t -> Uchar.t list
(** [lowercase_char c] converts a single character to lowercase. May return
    multiple characters for special cases. *)

val uppercase_char : Uchar.t -> Uchar.t list
(** [uppercase_char c] converts a single character to uppercase. May return
    multiple characters (e.g., ß -> SS). *)
