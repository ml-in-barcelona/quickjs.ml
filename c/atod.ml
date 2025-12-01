(** Low-level bindings to QuickJS's atod - ASCII to Double conversion

    JavaScript-compatible floating point parsing (atod = Ascii TO Double). These
    are raw C bindings; for a higher-level API, use [Quickjs.Global]. *)

(** Parse string to double with JS semantics. Flags: JS_ATOD_INT_ONLY |
    JS_ATOD_ACCEPT_BIN_OCT | etc. Sets *pnext to position after parsed number *)
let parse str pnext radix flags tmp_mem =
  Bindings.C.Functions.js_atod str pnext radix flags tmp_mem
