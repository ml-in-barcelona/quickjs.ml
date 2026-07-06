(** Low-level bindings to QuickJS's atod - ASCII to Double conversion

    JavaScript-compatible floating point parsing (atod = Ascii TO Double). These
    are raw C bindings; for a higher-level API, use [Quickjs.Global]. *)

(** Parse string to double with JS semantics. Flags: JS_ATOD_INT_ONLY |
    JS_ATOD_ACCEPT_BIN_OCT | etc. Sets [*poffset] to the number of bytes
    consumed (0 when parsing failed). Returns NaN when nothing could be parsed.
*)
let parse str poffset radix flags tmp_mem =
  Quickjs_bindings.C.Functions.js_atod str poffset radix flags tmp_mem
