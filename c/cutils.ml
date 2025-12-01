(** Low-level bindings to QuickJS's cutils - Integer to String Conversion

    Fast integer-to-string functions (itoa family). These are raw C bindings;
    for a higher-level API, use [Quickjs.Number]. *)

(** Convert unsigned 32-bit integer to decimal string *)
let u32toa buf n = Bindings.C.Functions.u32toa buf n

(** Convert signed 32-bit integer to decimal string *)
let i32toa buf n = Bindings.C.Functions.i32toa buf n

(** Convert unsigned 64-bit integer to decimal string *)
let u64toa buf n = Bindings.C.Functions.u64toa buf n

(** Convert signed 64-bit integer to decimal string *)
let i64toa buf n = Bindings.C.Functions.i64toa buf n

(** Convert unsigned 64-bit integer to string in given radix (2-36) *)
let u64toa_radix buf n radix = Bindings.C.Functions.u64toa_radix buf n radix

(** Convert signed 64-bit integer to string in given radix (2-36) *)
let i64toa_radix buf n radix = Bindings.C.Functions.i64toa_radix buf n radix
