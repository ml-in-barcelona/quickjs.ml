type t
(** The RegExp object *)

type result
(** The result of a executing a RegExp on a string *)

type compile_error =
  [ `Unexpected_end
  | `Malformed_unicode_char
  | `Invalid_escape_sequence
  | `Nothing_to_repeat
  | `Unknown of string ]
(** Possible errors when compiling a RegExp pattern *)

val compile_error_to_string : compile_error -> string
(** Convert a compile error to a human-readable string *)

val compile : flags:string -> string -> (t, compile_error) Stdlib.result
(** Constructs a RegExp.t from a string describing a regex and their flags.
    Returns [Error (error_type, raw_message)] if compilation fails. *)

val lastIndex : t -> int
(** returns the index where the next match will start its search *)

val setLastIndex : t -> int -> unit
(** sets the index at which the next match (RegExp.exec or RegExp.test) will
    start its search from *)

val flags : t -> string
(** returns the enabled flags as a string *)

val global : t -> bool
(** returns a bool indicating whether the global flag (g) is set *)

val ignorecase : t -> bool
(** returns a bool indicating whether the ignorecase (i) flag is set *)

val multiline : t -> bool
(** returns a bool indicating whether the multiline (m) flag is set *)

val dotall : t -> bool
(** returns a bool indicating whether the dot all (s) flag is set *)

val sticky : t -> bool
(** returns a bool indicating whether the sticky (y) flag is set *)

val unicode : t -> bool
(** returns a bool indicating whether the unicode (u ) flag is set *)

val source : t -> string
(** returns the regexp pattern as a string *)

val test : t -> string -> bool
(** checks whether the given RegExp.t will match (or not match) a given string
*)

val exec : t -> string -> result
(** executes a search on a given string using the given RegExp.t *)

val captures : result -> string array
(** an array of the match and captures *)

val groups : result -> (string * string) list
(** returns all named capture groups as a list of (name, value) pairs *)

val group : string -> result -> string option
(** returns the value of a named capture group, or None if not found *)

val input : result -> string
(** the original input string *)

val index : result -> int
(** sets the index at which the next match (RegExp.exec or RegExp.test) will
    start its search from *)
