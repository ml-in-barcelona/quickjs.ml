(** Number parsing following JavaScript's parseInt and parseFloat semantics *)

val parseInt : ?radix:int -> string -> float
(** [parseInt ?radix str] parses a string argument and returns an integer of the
    specified radix.

    This follows JavaScript's parseInt specification:
    - Leading whitespace is ignored
    - An optional sign (+/-) is handled
    - If radix is 0 or not provided, the radix is determined by the string:
      - Strings starting with "0x" or "0X" are parsed as hexadecimal (radix 16)
      - All other strings are parsed as decimal (radix 10)
    - If radix is outside the range 2-36, returns nan
    - Parsing stops at the first character that is not a valid digit in the radix
    - If no valid digits are found, returns nan

    @param radix The radix (base) to use, between 2 and 36 inclusive, or 0 for
    auto-detection (default: 0)
    @param str The string to parse
    @return The parsed integer as a float, or nan if parsing fails *)

val parseFloat : string -> float
(** [parseFloat str] parses a string argument and returns a floating point number.

    This follows JavaScript's parseFloat specification:
    - Leading whitespace is ignored
    - An optional sign (+/-) is handled
    - Handles "Infinity" and "-Infinity"
    - Handles scientific notation (e.g., "1e10", "1.5e-3")
    - Parsing stops at the first character that cannot be part of a number
    - If no valid number is found, returns nan

    @param str The string to parse
    @return The parsed float, or nan if parsing fails *)

val isNaN : float -> bool
(** [isNaN value] determines whether a value is NaN.
    @param value The value to test
    @return true if the value is NaN, false otherwise *)

val isFinite : float -> bool
(** [isFinite value] determines whether a value is a finite number.
    @param value The value to test
    @return true if the value is finite, false otherwise (for NaN or infinity) *)

val isInteger : float -> bool
(** [isInteger value] determines whether a value is an integer.
    @param value The value to test
    @return true if the value is an integer, false otherwise *)
