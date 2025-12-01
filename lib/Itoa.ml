(** Fast integer-to-string conversion

    This module provides optimized integer-to-string conversion using QuickJS's
    C implementation. *)

(* Maximum buffer size for integer conversions *)
let max_int_buf_size = 65 (* 64 bits in binary + sign *)

let of_int32 n =
  let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
  let len = Bindings.C.Functions.i32toa buf n in
  Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

let of_uint32 n =
  let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
  let len = Bindings.C.Functions.u32toa buf n in
  Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

let of_int64 n =
  let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
  let len = Bindings.C.Functions.i64toa buf n in
  Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

let of_uint64 n =
  let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
  let len = Bindings.C.Functions.u64toa buf n in
  Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

let of_int n = of_int64 (Int64.of_int n)

let of_int32_radix ~radix n =
  if radix < 2 || radix > 36 then
    invalid_arg
      (Printf.sprintf
         "Itoa.of_int32_radix: radix must be between 2 and 36, got %d" radix);
  let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
  let len = Bindings.C.Functions.i64toa_radix buf (Int64.of_int32 n) radix in
  Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

let of_int64_radix ~radix n =
  if radix < 2 || radix > 36 then
    invalid_arg
      (Printf.sprintf
         "Itoa.of_int64_radix: radix must be between 2 and 36, got %d" radix);
  let buf = Ctypes.allocate_n Ctypes.char ~count:max_int_buf_size in
  let len = Bindings.C.Functions.i64toa_radix buf n radix in
  Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int len)

let of_int_radix ~radix n = of_int64_radix ~radix (Int64.of_int n)

