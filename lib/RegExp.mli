type regex
type result

val global : int -> bool
val ignorecase : int -> bool
val multiline : int -> bool
val dotall : int -> bool
val sticky : int -> bool
val parse_flags : string -> int
val compile : string -> string -> regex
val lastIndex : regex -> int
val exec : regex -> string -> result
val captures : result -> string array
