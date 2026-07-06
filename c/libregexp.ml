(** Low-level bindings to QuickJS's libregexp - Regular Expression Engine

    ES2023-compliant regex engine with Unicode support. These are raw C
    bindings; for a higher-level API, use [Quickjs.RegExp]. *)

(** Compile a regular expression pattern into bytecode. Returns pointer to
    bytecode buffer, or NULL on error with message in error_msg. The buffer must
    be released with [bytecode_free]. *)
let compile plen error_msg error_msg_size buf buf_len flags opaque =
  Quickjs_bindings.C.Functions.lre_compile plen error_msg error_msg_size buf
    buf_len flags opaque

(** Execute a compiled regex against input string. [deadline] is an optional
    pointer to a monotonic deadline in milliseconds (see [now_ms]). Returns: 1 =
    match, 0 = no match, -1 = memory error, -2 = timeout *)
let exec capture bc_buf cbuf cindex clen cbuf_type deadline =
  Quickjs_bindings.C.Functions.lre_exec capture bc_buf cbuf cindex clen
    cbuf_type deadline

(** Free a bytecode buffer returned by [compile] *)
let bytecode_free bc_buf = Quickjs_bindings.C.Functions.lre_bytecode_free bc_buf

(** Monotonic clock in milliseconds; basis for [exec] deadlines *)
let now_ms () = Quickjs_bindings.C.Functions.lre_now_ms ()

(** Get number of capture groups (including group 0 for full match) *)
let get_capture_count bc_buf =
  Quickjs_bindings.C.Functions.lre_get_capture_count bc_buf

(** Get pointer to null-terminated group names *)
let get_groupnames bc_buf =
  Quickjs_bindings.C.Functions.lre_get_groupnames bc_buf

(** Get flags from compiled bytecode *)
let get_flags bc_buf = Quickjs_bindings.C.Functions.lre_get_flags bc_buf
