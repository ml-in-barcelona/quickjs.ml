type regex
type result

val global : int -> bool
val ignorecase : int -> bool
val multiline : int -> bool
val dotall : int -> bool
val sticky : int -> bool
val parse_flags : string -> int
val compile : string -> string -> regex
val index : regex -> int
val setLastIndex : regex -> int -> unit
val exec : regex -> string -> result
val captures : result -> string array
val flags : regex -> string
