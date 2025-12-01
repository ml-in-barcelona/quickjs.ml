(** Low-level bindings to QuickJS's dtoa - Double to ASCII conversion

    JavaScript-compatible floating point formatting (dtoa = Double TO Ascii).
    These are raw C bindings; for a higher-level API, use [Quickjs.Number]. *)

(** Calculate maximum buffer size needed for [to_string] *)
let max_len d radix n_digits flags =
  Bindings.C.Functions.js_dtoa_max_len d radix n_digits flags

(** Convert double to string with JS semantics. Flags: JS_DTOA_FORMAT_* |
    JS_DTOA_EXP_* | JS_DTOA_MINUS_ZERO Returns actual string length *)
let to_string buf d radix n_digits flags tmp_mem =
  Bindings.C.Functions.js_dtoa buf d radix n_digits flags tmp_mem
