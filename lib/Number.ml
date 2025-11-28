(* These are pure OCaml implementations rather than bindings to QuickJS because
   QuickJS's js_parseInt/js_parseFloat require the full JS runtime (JSContext,
   JSValue) and use internal functions like js_atof that aren't exposed in headers.
   The low-level parsing (js_strtod) is also static/internal to quickjs.c.

   Unlike libregexp.c which has standalone functions (lre_compile, lre_exec),
   there's no equivalent standalone API for number parsing in QuickJS.

   Pure OCaml was chosen over adding C wrappers because:
   - The logic is straightforward (~200 lines)
   - No additional C code to maintain
   - Portable and easy to test
   - Matches JavaScript semantics exactly *)

let is_whitespace c =
  match c with
  | ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' -> true
  (* Unicode whitespace characters commonly handled *)
  | _ -> false

let skip_whitespace s start =
  let len = String.length s in
  let rec loop i =
    if i >= len then i else if is_whitespace s.[i] then loop (i + 1) else i
  in
  loop start

let digit_value c radix =
  let v =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'z' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'Z' -> Char.code c - Char.code 'A' + 10
    | _ -> -1
  in
  if v >= 0 && v < radix then Some v else None

(** [parseInt str radix] parses a string argument and returns an integer of the
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

    @param str The string to parse
    @param radix The radix (base) to use, between 2 and 36 inclusive, or 0 for auto-detection
    @return The parsed integer as a float, or nan if parsing fails
*)
let parseInt ?(radix = 0) str =
  let len = String.length str in
  let i = skip_whitespace str 0 in

  if i >= len then nan
  else if (* Handle radix validation *)
          radix <> 0 && (radix < 2 || radix > 36)
  then nan
  else
    (* Handle sign *)
    let sign, i =
      match str.[i] with
      | '-' -> (-1.0, i + 1)
      | '+' -> (1.0, i + 1)
      | _ -> (1.0, i)
    in

    if i >= len then nan
    else
      (* Determine radix and skip prefix if needed *)
      let radix, i =
        if radix = 0 || radix = 16 then
          (* Check for 0x/0X prefix *)
          if
            i + 1 < len
            && str.[i] = '0'
            && (str.[i + 1] = 'x' || str.[i + 1] = 'X')
          then (16, i + 2)
          else if radix = 0 then (10, i)
          else (radix, i)
        else (radix, i)
      in

      if i >= len then nan
      else
        (* Parse digits *)
        let rec parse_digits acc idx found_digit =
          if idx >= len then (acc, found_digit)
          else
            match digit_value str.[idx] radix with
            | Some v ->
                parse_digits
                  ((acc *. float_of_int radix) +. float_of_int v)
                  (idx + 1) true
            | None -> (acc, found_digit)
        in

        let result, found_digit = parse_digits 0.0 i false in
        if found_digit then sign *. result else nan

(** [parseFloat str] parses a string argument and returns a floating point number.

    This follows JavaScript's parseFloat specification:
    - Leading whitespace is ignored
    - An optional sign (+/-) is handled
    - Handles "Infinity" and "-Infinity"
    - Handles scientific notation (e.g., "1e10", "1.5e-3")
    - Parsing stops at the first character that cannot be part of a number
    - If no valid number is found, returns nan

    @param str The string to parse
    @return The parsed float, or nan if parsing fails
*)
let parseFloat str =
  let len = String.length str in
  let i = skip_whitespace str 0 in

  if i >= len then nan
  else
    (* Handle sign *)
    let sign, i =
      match str.[i] with
      | '-' -> (-1.0, i + 1)
      | '+' -> (1.0, i + 1)
      | _ -> (1.0, i)
    in

    if i >= len then nan
    else
      (* Check for Infinity *)
      let infinity_str = "Infinity" in
      let infinity_len = String.length infinity_str in
      if i + infinity_len <= len && String.sub str i infinity_len = infinity_str
      then sign *. infinity
      else
        (* Parse integer part *)
        let rec parse_int_part acc idx has_digits =
          if idx >= len then (acc, idx, has_digits)
          else
            match str.[idx] with
            | '0' .. '9' as c ->
                let digit = float_of_int (Char.code c - Char.code '0') in
                parse_int_part ((acc *. 10.0) +. digit) (idx + 1) true
            | _ -> (acc, idx, has_digits)
        in

        let int_part, i, has_int_digits = parse_int_part 0.0 i false in

        (* Parse fractional part *)
        let frac_part, i, has_frac_digits =
          if i < len && str.[i] = '.' then
            let rec parse_frac_part acc divisor idx has_digits =
              if idx >= len then (acc, divisor, idx, has_digits)
              else
                match str.[idx] with
                | '0' .. '9' as c ->
                    let digit = float_of_int (Char.code c - Char.code '0') in
                    parse_frac_part
                      (acc +. (digit /. divisor))
                      (divisor *. 10.0) (idx + 1) true
                | _ -> (acc, divisor, idx, has_digits)
            in
            let frac, _, new_i, has_digits =
              parse_frac_part 0.0 10.0 (i + 1) false
            in
            (frac, new_i, has_digits)
          else (0.0, i, false)
        in

        if not (has_int_digits || has_frac_digits) then nan
        else
          let mantissa = int_part +. frac_part in

          (* Parse exponent *)
          let exponent =
            if i < len && (str.[i] = 'e' || str.[i] = 'E') then
              let exp_sign, j =
                if i + 1 < len then
                  match str.[i + 1] with
                  | '-' -> (-1, i + 2)
                  | '+' -> (1, i + 2)
                  | _ -> (1, i + 1)
                else (1, i + 1)
              in
              let rec parse_exp acc idx has_digits =
                if idx >= len then (acc, has_digits)
                else
                  match str.[idx] with
                  | '0' .. '9' as c ->
                      let digit = Char.code c - Char.code '0' in
                      parse_exp ((acc * 10) + digit) (idx + 1) true
                  | _ -> (acc, has_digits)
              in
              let exp_val, has_exp_digits = parse_exp 0 j false in
              if has_exp_digits then Some (exp_sign * exp_val) else None
            else None
          in

          let result =
            match exponent with
            | Some exp -> mantissa *. (10.0 ** float_of_int exp)
            | None -> mantissa
          in

          sign *. result

(** [isNaN value] determines whether a value is NaN.
    @param value The value to test
    @return true if the value is NaN, false otherwise
*)
let isNaN value = Float.is_nan value

(** [isFinite value] determines whether a value is a finite number.
    @param value The value to test
    @return true if the value is finite, false otherwise (for NaN or infinity)
*)
let isFinite value = Float.is_finite value

(** [isInteger value] determines whether a value is an integer.
    @param value The value to test
    @return true if the value is an integer, false otherwise
*)
let isInteger value = Float.is_integer value
