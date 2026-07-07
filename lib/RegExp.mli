(** JavaScript RegExp built-in object, backed by QuickJS's libregexp.

    {2 Index units}

    Strings are UTF-8 encoded OCaml strings, but every index exposed by this
    module ([index], [last_index], [set_last_index]) is a
    {b UTF-16 code unit offset}, exactly like JavaScript's RegExp. This keeps
    results consistent with {!Quickjs.String}, whose indices are UTF-16 as well.

    {2 Lifetime and state}

    A compiled regexp owns C-allocated bytecode which is released automatically
    when the value is garbage collected. Like in JavaScript, regexps compiled
    with the global ([g]) or sticky ([y]) flag carry mutable matching state
    ([last_index]); a [t] value is therefore not safe to share between threads
    without synchronization. *)

type t
(** The RegExp object *)

type match_indices = {
  ranges : (int * int) option array;
      (** Entry [g] is the [(start, end_)] range of capture group [g] in UTF-16
          code units, with [end_] exclusive (the same convention as JavaScript's
          [match.indices]: [start] is the index of the first code unit of the
          capture and [end_] the index after the last one). Entry 0 is the full
          match. A group that did not participate is [None]. *)
  groups : (string * (int * int) option) list;
      (** Ranges of named capture groups in source order, like JavaScript's
          [match.indices.groups], with [None] for groups that did not
          participate. *)
}
(** Match positions, equivalent to JavaScript's [match.indices] (the RegExp
    Match Indices proposal, ES2022). *)

type match_result = {
  captures : string option array;
      (** Entry 0 is the full match; entries 1..n are capture groups. A group
          that did not participate in the match is [None] (JavaScript's
          [undefined]), which is distinct from a group that matched the empty
          string ([Some ""]). *)
  index : int;  (** UTF-16 index of the match start in [input]. *)
  input : string;  (** The input string that was matched against. *)
  groups : (string * string option) list;
      (** Named capture groups in source order, with [None] for groups that did
          not participate. *)
  indices : match_indices option;
      (** Capture group positions. [Some] iff the regexp was compiled with the
          [d] flag, like JavaScript's [hasIndices]. *)
}
(** The result of a successful match. *)

type compile_error =
  [ `Unexpected_end
  | `Malformed_unicode_char
  | `Invalid_escape_sequence
  | `Nothing_to_repeat
  | `Stack_overflow
  | `Invalid_flags of string
  | `Unknown of string ]
(** Possible errors when compiling a RegExp pattern. [`Invalid_flags] is
    returned for unknown flags, duplicated flags, or combining [u] with [v].
    [`Stack_overflow] is returned for patterns nested too deeply to compile. *)

exception Timeout
(** Raised by {!exec} and {!test} when the [timeout_ms] budget is exhausted. *)

val compile_error_to_string : compile_error -> string
(** Convert a compile error to a human-readable string *)

val compile : flags:string -> string -> (t, compile_error) Stdlib.result
(** [compile ~flags source] compiles [source] with the given JavaScript flags
    (any of ["dgimsuvy"], each at most once; [u] and [v] are mutually
    exclusive).

    Without the [u]/[v] flag the pattern is matched as UTF-16 code units (astral
    code points behave as surrogate pairs), mirroring JavaScript's non-unicode
    regexp semantics. *)

val last_index : t -> int
(** Returns the UTF-16 index where the next match will start its search. Only
    meaningful for regexps compiled with the global ([g]) or sticky ([y]) flag;
    it stays [0] otherwise, like in JavaScript. *)

val set_last_index : t -> int -> unit
(** Sets the UTF-16 index at which the next {!exec} or {!test} on a global or
    sticky regexp starts its search. Negative values are clamped to [0]
    (JavaScript's ToLength coercion). *)

val flags : t -> string
(** Returns the enabled flags in canonical order ("dgimsuvy" subset), exactly as
    passed to {!compile}. *)

val global : t -> bool
(** whether the global flag (g) is set *)

val ignorecase : t -> bool
(** whether the ignorecase flag (i) is set *)

val multiline : t -> bool
(** whether the multiline flag (m) is set *)

val dotall : t -> bool
(** whether the dotall flag (s) is set *)

val sticky : t -> bool
(** whether the sticky flag (y) is set *)

val unicode : t -> bool
(** whether the unicode flag (u) is set *)

val unicode_sets : t -> bool
(** whether the unicode sets flag (v) is set *)

val indices : t -> bool
(** whether the indices flag (d) is set *)

val source : t -> string
(** returns the regexp pattern as a string *)

val exec : ?timeout_ms:float -> t -> string -> match_result option
(** [exec regexp input] executes a search and returns the first match, or [None]
    when there is no match.

    For global/sticky regexps the search starts at {!last_index}, and
    {!last_index} is updated to the end of the match (or reset to [0] on no
    match), enabling JavaScript-style match iteration.

    [timeout_ms] bounds the execution time of the underlying engine; when
    exhausted, {!Timeout} is raised. Without it, pathological patterns can
    backtrack for a very long time.

    @raise Timeout when [timeout_ms] is exceeded.
    @raise Out_of_memory when the engine fails to allocate. *)

val test : ?timeout_ms:float -> t -> string -> bool
(** [test regexp input] is [exec regexp input <> None]. Like in JavaScript, it
    advances {!last_index} on global/sticky regexps.

    @raise Timeout when [timeout_ms] is exceeded.
    @raise Out_of_memory when the engine fails to allocate. *)

val group : string -> match_result -> string option
(** [group name m] returns the value of named capture group [name], or [None]
    when the group does not exist or did not participate in the match. *)

val group_indices : string -> match_result -> (int * int) option
(** [group_indices name m] returns the UTF-16 [(start, end_)] range of named
    capture group [name] ([end_] exclusive), or [None] when the regexp was
    compiled without the [d] flag, the group does not exist, or it did not
    participate in the match. Equivalent to JavaScript's
    [match.indices.groups[name]]. *)
