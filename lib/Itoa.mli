(** Fast integer-to-string conversion

    This module provides optimized integer-to-string conversion using QuickJS's
    C implementation. *)

val of_int32 : int32 -> string
(** Convert int32 to decimal string *)

val of_uint32 : Unsigned.uint32 -> string
(** Convert uint32 to decimal string *)

val of_int64 : int64 -> string
(** Convert int64 to decimal string *)

val of_uint64 : Unsigned.uint64 -> string
(** Convert uint64 to decimal string *)

val of_int : int -> string
(** Convert int to decimal string *)

val of_int32_radix : radix:int -> int32 -> string
(** Convert int32 to string in given radix (2-36).

    @raise Invalid_argument if radix is not in range 2-36 *)

val of_int64_radix : radix:int -> int64 -> string
(** Convert int64 to string in given radix (2-36).

    @raise Invalid_argument if radix is not in range 2-36 *)

val of_int_radix : radix:int -> int -> string
(** Convert int to string in given radix (2-36).

    @raise Invalid_argument if radix is not in range 2-36 *)

