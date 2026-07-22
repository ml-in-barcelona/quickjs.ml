(* JavaScript RegExp built-in object, backed by QuickJS's libregexp.

   Unit conventions: every index that crosses this module's public API
   ([index], [last_index], [set_last_index]) is a UTF-16 code unit offset,
   exactly like JavaScript's RegExp. Strings themselves are UTF-8 encoded
   OCaml strings; conversions happen internally. *)

module Libregexp = Quickjs_c.Libregexp

type bytecode =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  bc_storage : bytecode; [@warning "-69"]
      (* Owns the compiled bytecode. Stored in a Bigarray so the GC accounts
         for the memory (lre_compile's malloc'd buffer is copied here and
         freed immediately); [bc] points into it and keeps it alive. *)
  bc : Unsigned.uint8 Ctypes_static.ptr;
  source : string;
  flags : int; (* flags as requested by the user *)
  mutable last_index : int; (* UTF-16 code units *)
}

type match_indices = {
  ranges : (int * int) option array; (* UTF-16 [start, end) *)
  groups : (string * (int * int) option) list;
}

type match_result = {
  captures : string option array;
  index : int; (* UTF-16 code units *)
  input : string;
  groups : (string * string option) list;
  indices : match_indices option; (* Some iff the 'd' flag is set *)
}

type source_range = { utf16 : int * int; bytes : (int * int) option }
type prepared_match = { result : match_result; range : source_range }

type compile_error =
  [ `Unexpected_end
  | `Malformed_unicode_char
  | `Invalid_escape_sequence
  | `Nothing_to_repeat
  | `Stack_overflow
  | `Invalid_flags of string
  | `Unknown of string ]

exception Timeout

let compile_error_to_string = function
  | `Unexpected_end -> "unexpected end"
  | `Malformed_unicode_char -> "malformed unicode char"
  | `Invalid_escape_sequence -> "invalid escape sequence"
  | `Nothing_to_repeat -> "nothing to repeat"
  | `Stack_overflow -> "stack overflow"
  | `Invalid_flags msg -> msg
  | `Unknown s -> s

(* LRE_FLAG_* values from libregexp.h *)
let lre_flag_global = 1 lsl 0
let lre_flag_ignorecase = 1 lsl 1
let lre_flag_multiline = 1 lsl 2
let lre_flag_dotall = 1 lsl 3
let lre_flag_unicode = 1 lsl 4
let lre_flag_sticky = 1 lsl 5
let lre_flag_indices = 1 lsl 6
let lre_flag_unicode_sets = 1 lsl 8
let has_flag flags flag = flags land flag != 0
let global regexp = has_flag regexp.flags lre_flag_global
let ignorecase regexp = has_flag regexp.flags lre_flag_ignorecase
let multiline regexp = has_flag regexp.flags lre_flag_multiline
let dotall regexp = has_flag regexp.flags lre_flag_dotall
let sticky regexp = has_flag regexp.flags lre_flag_sticky
let unicode regexp = has_flag regexp.flags lre_flag_unicode
let unicode_sets regexp = has_flag regexp.flags lre_flag_unicode_sets
let indices regexp = has_flag regexp.flags lre_flag_indices

(* JavaScript regular expression flags, in canonical order (the order
   returned by JavaScript's RegExp.prototype.flags). *)
let known_flags =
  [
    ('d', lre_flag_indices);
    ('g', lre_flag_global);
    ('i', lre_flag_ignorecase);
    ('m', lre_flag_multiline);
    ('s', lre_flag_dotall);
    ('u', lre_flag_unicode);
    ('v', lre_flag_unicode_sets);
    ('y', lre_flag_sticky);
  ]

(* Parse a JavaScript flags string. Mirrors QuickJS's js_compile_regexp:
   unknown flags, duplicated flags and combining 'u' with 'v' are errors. *)
let parse_flags flags =
  let rec loop i acc =
    if i >= Stdlib.String.length flags then Ok acc
    else
      let c = Stdlib.String.get flags i in
      match List.assoc_opt c known_flags with
      | None ->
          Error (`Invalid_flags (Printf.sprintf "unknown regexp flag '%c'" c))
      | Some mask when has_flag acc mask ->
          Error
            (`Invalid_flags (Printf.sprintf "duplicated regexp flag '%c'" c))
      | Some mask -> loop (i + 1) (acc lor mask)
  in
  match loop 0 0 with
  | Error _ as error -> error
  | Ok acc
    when has_flag acc lre_flag_unicode && has_flag acc lre_flag_unicode_sets ->
      Error (`Invalid_flags "regexp flags 'u' and 'v' cannot be combined")
  | Ok acc -> Ok acc

let flags_to_string flags =
  Stdlib.String.concat ""
    (List.filter_map
       (fun (c, mask) ->
         if has_flag flags mask then Some (Stdlib.String.make 1 c) else None)
       known_flags)

let strlen ptr =
  let rec aux ptr len =
    let c = Ctypes.( !@ ) ptr in
    if c = char_of_int 0 then len else aux (Ctypes.( +@ ) ptr 1) (len + 1)
  in
  aux ptr 0

(* Check whether a string contains code points >= U+10000. In valid UTF-8
   those are exactly the 4-byte sequences, whose lead byte is >= 0xF0. *)
let has_astral_chars s =
  let len = Stdlib.String.length s in
  let rec check i =
    if i >= len then false
    else if Char.code (Stdlib.String.get s i) >= 0xF0 then true
    else check (i + 1)
  in
  check 0

(* Check if a string contains only ASCII bytes. *)
let is_ascii s =
  let len = Stdlib.String.length s in
  let rec check i =
    if i >= len then true
    else if Char.code (Stdlib.String.get s i) >= 0x80 then false
    else check (i + 1)
  in
  check 0

(* Encode a string as CESU-8: like UTF-8, except code points >= U+10000 are
   encoded as a UTF-16 surrogate pair, each surrogate encoded as a 3-byte
   sequence. This is what QuickJS feeds lre_compile for regexps without the
   'u'/'v' flag (see JS_ToCStringLen2 in quickjs.c): the pattern is then a
   sequence of UTF-16 code units, matching JavaScript's non-unicode regexp
   semantics exactly (libregexp rejects raw astral code points otherwise). *)
let to_cesu8 s =
  let len = Stdlib.String.length s in
  let buf = Stdlib.Buffer.create (len + 8) in
  let add_code_unit code =
    (* code <= 0xFFFF; 3-byte encoding also covers surrogate values, which
       Buffer.add_utf_8_uchar would reject *)
    if code < 0x80 then Stdlib.Buffer.add_char buf (Char.chr code)
    else if code < 0x800 then begin
      Stdlib.Buffer.add_char buf (Char.chr (0xC0 lor (code lsr 6)));
      Stdlib.Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
    end
    else begin
      Stdlib.Buffer.add_char buf (Char.chr (0xE0 lor (code lsr 12)));
      Stdlib.Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
      Stdlib.Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
    end
  in
  let rec loop i =
    if i >= len then Stdlib.Buffer.contents buf
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
      if code < 0x10000 then add_code_unit code
      else begin
        let code' = code - 0x10000 in
        add_code_unit (0xD800 lor (code' lsr 10));
        add_code_unit (0xDC00 lor (code' land 0x3FF))
      end;
      loop (i + Uchar.utf_decode_length d)
  in
  loop 0

let compile ~flags:flags_string source =
  match parse_flags flags_string with
  | Error _ as error -> error
  | Ok flags -> (
      let compiled_byte_code_len = Ctypes.allocate Ctypes.int 0 in
      let size_of_error_msg = 64 in
      let error_msg = Ctypes.allocate_n ~count:size_of_error_msg Ctypes.char in
      (* Without 'u'/'v', JavaScript treats the pattern as UTF-16 code units:
         astral code points become surrogate pairs. libregexp expects the
         pattern in CESU-8 in that mode, and in UTF-8 in unicode mode. *)
      let is_unicode_mode =
        has_flag flags lre_flag_unicode || has_flag flags lre_flag_unicode_sets
      in
      let pattern =
        if (not is_unicode_mode) && has_astral_chars source then to_cesu8 source
        else source
      in
      let input = Ctypes.ocaml_string_start pattern in
      let input_length =
        Stdlib.String.length pattern |> Unsigned.Size_t.of_int
      in
      let compiled_byte_code =
        Libregexp.compile compiled_byte_code_len error_msg size_of_error_msg
          input input_length flags Ctypes.null
      in
      match Ctypes.is_null compiled_byte_code with
      | false ->
          (* Copy the bytecode into GC-accounted storage (Bigarray) and free
             the C-allocated buffer right away: the GC cannot see C-side
             memory, so keeping it alive behind a finalizer lets allocation
             debt grow unboundedly under light OCaml heap pressure. *)
          let bc_len = Ctypes.( !@ ) compiled_byte_code_len in
          let bc_storage =
            Bigarray.Array1.create Bigarray.char Bigarray.c_layout bc_len
          in
          let src_chars =
            Ctypes.coerce
              (Ctypes.ptr Ctypes.uint8_t)
              (Ctypes.ptr Ctypes.char) compiled_byte_code
          in
          for i = 0 to bc_len - 1 do
            Bigarray.Array1.unsafe_set bc_storage i
              (Ctypes.( !@ ) (Ctypes.( +@ ) src_chars i))
          done;
          Libregexp.bytecode_free compiled_byte_code;
          let bc =
            Ctypes.coerce (Ctypes.ptr Ctypes.char)
              (Ctypes.ptr Ctypes.uint8_t)
              (Ctypes.bigarray_start Ctypes.array1 bc_storage)
          in
          Ok { bc_storage; bc; flags; last_index = 0; source }
      | true ->
          let length = strlen error_msg in
          let error = Ctypes.string_from_ptr ~length error_msg in
          Error
            (match error with
            | "unexpected end" -> `Unexpected_end
            | "malformed unicode char" -> `Malformed_unicode_char
            | "nothing to repeat" -> `Nothing_to_repeat
            | "stack overflow" -> `Stack_overflow
            | "invalid escape sequence in regular expression" ->
                `Invalid_escape_sequence
            | unknown -> `Unknown unknown))

let last_index regexp = regexp.last_index

(* JavaScript coerces lastIndex with ToLength, which clamps negative values
   to 0. Without the clamp a negative index would reach lre_exec as an
   out-of-bounds read. *)
let set_last_index regexp idx = regexp.last_index <- max 0 idx
let source regexp = regexp.source
let flags regexp = flags_to_string regexp.flags

let group name (result : match_result) =
  match List.assoc_opt name result.groups with
  | Some value -> value
  | None -> None

let group_indices name (result : match_result) =
  match result.indices with
  | None -> None
  | Some indices -> (
      match List.assoc_opt name indices.groups with
      | Some value -> value
      | None -> None)

(* Convert a UTF-8 string to UTF-16 code units, packed as bytes in the
   platform's native endianness (lre_exec reads the 16-bit buffer through
   native uint16_t loads). *)
let utf8_to_utf16_bytes s =
  let len = Stdlib.String.length s in
  let buf = Stdlib.Buffer.create (len * 2) in
  let add_u16 code =
    let lo = Char.chr (code land 0xFF) in
    let hi = Char.chr ((code lsr 8) land 0xFF) in
    if Sys.big_endian then begin
      Stdlib.Buffer.add_char buf hi;
      Stdlib.Buffer.add_char buf lo
    end
    else begin
      Stdlib.Buffer.add_char buf lo;
      Stdlib.Buffer.add_char buf hi
    end
  in
  let rec loop i =
    if i >= len then Stdlib.Buffer.contents buf
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
      (if code < 0x10000 then add_u16 code
       else
         let code' = code - 0x10000 in
         add_u16 (0xD800 lor (code' lsr 10));
         add_u16 (0xDC00 lor (code' land 0x3FF)));
      loop (i + Uchar.utf_decode_length d)
  in
  loop 0

(* Exact UTF-8 byte offset for each UTF-16 boundary. An entry is [-1] when the
   corresponding UTF-16 index falls between the surrogates of an astral code
   point. The final entry is the source byte length. *)
type utf16_map = { u8_by_u16 : int array; valid_utf8 : bool }

let build_utf16_map s =
  let len = Stdlib.String.length s in
  let rec count i units =
    if i >= len then units
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
      count
        (i + Uchar.utf_decode_length d)
        (units + if code >= 0x10000 then 2 else 1)
  in
  let u8_by_u16 = Array.make (count 0 0 + 1) (-1) in
  let valid_utf8 = ref true in
  let rec fill i u16_idx =
    if i >= len then begin
      u8_by_u16.(u16_idx) <- i
    end
    else begin
      let d = Stdlib.String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then valid_utf8 := false;
      let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
      u8_by_u16.(u16_idx) <- i;
      let units = if code >= 0x10000 then 2 else 1 in
      fill (i + Uchar.utf_decode_length d) (u16_idx + units)
    end
  in
  fill 0 0;
  { u8_by_u16; valid_utf8 = !valid_utf8 }

let utf16_length_of_map map = Array.length map.u8_by_u16 - 1

let utf16_to_utf8_exact map target =
  if target < 0 || target > utf16_length_of_map map then None
  else
    let byte_index = map.u8_by_u16.(target) in
    if byte_index < 0 then None else Some byte_index

let fill_carray_of_string s =
  let len = Stdlib.String.length s in
  let arr = Ctypes.CArray.make Ctypes.uint8_t len in
  for i = 0 to len - 1 do
    Ctypes.CArray.set arr i
      (Unsigned.UInt8.of_int (Char.code (Stdlib.String.get s i)))
  done;
  arr

type prepared_input = {
  input : string;
  buffer_storage : Unsigned.uint8 Ctypes.CArray.t;
  matching_length : int;
  buffer_type : int;
  utf16_map : utf16_map option;
}

let prepare_input input =
  if is_ascii input then
    {
      input;
      buffer_storage = fill_carray_of_string input;
      matching_length = Stdlib.String.length input;
      buffer_type = 0;
      utf16_map = None;
    }
  else
    let map = build_utf16_map input in
    let utf16_str = utf8_to_utf16_bytes input in
    {
      input;
      buffer_storage = fill_carray_of_string utf16_str;
      matching_length = utf16_length_of_map map;
      buffer_type = 1;
      utf16_map = Some map;
    }

let prepared_byte_range prepared ~start ~end_ =
  if start < 0 || end_ < start || end_ > prepared.matching_length then None
  else
    match prepared.utf16_map with
    | None -> Some (start, end_)
    | Some map when not map.valid_utf8 -> None
    | Some map -> (
        match (utf16_to_utf8_exact map start, utf16_to_utf8_exact map end_) with
        | Some start_byte, Some end_byte -> Some (start_byte, end_byte)
        | _ -> None)

let utf16_unit prepared index =
  let offset = index * 2 in
  let first =
    Ctypes.CArray.get prepared.buffer_storage offset |> Unsigned.UInt8.to_int
  in
  let second =
    Ctypes.CArray.get prepared.buffer_storage (offset + 1)
    |> Unsigned.UInt8.to_int
  in
  if Sys.big_endian then (first lsl 8) lor second else first lor (second lsl 8)

let prepared_substring prepared ~start ~end_ =
  let start = Stdlib.max 0 (Stdlib.min start prepared.matching_length) in
  let end_ = Stdlib.max start (Stdlib.min end_ prepared.matching_length) in
  match prepared_byte_range prepared ~start ~end_ with
  | Some (start_byte, end_byte) ->
      Stdlib.String.sub prepared.input start_byte (end_byte - start_byte)
  | None ->
      let buf = Stdlib.Buffer.create ((end_ - start) * 3) in
      let rec loop index =
        if index < end_ then (
          let code = utf16_unit prepared index in
          if code >= 0xD800 && code <= 0xDBFF && index + 1 < end_ then
            let low = utf16_unit prepared (index + 1) in
            if low >= 0xDC00 && low <= 0xDFFF then (
              let code_point =
                0x10000 + ((code - 0xD800) * 0x400) + (low - 0xDC00)
              in
              Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_int code_point);
              loop (index + 2))
            else (
              Stdlib.Buffer.add_utf_8_uchar buf Uchar.rep;
              loop (index + 1))
          else
            let uchar =
              if code >= 0xD800 && code <= 0xDFFF then Uchar.rep
              else Uchar.of_int code
            in
            Stdlib.Buffer.add_utf_8_uchar buf uchar;
            loop (index + 1))
      in
      loop start;
      Stdlib.Buffer.contents buf

let prepared_advance_index prepared ~unicode index =
  let index = Stdlib.max 0 index in
  let next = if index = Stdlib.max_int then index else index + 1 in
  if
    (not unicode) || prepared.buffer_type = 0
    || next >= prepared.matching_length
  then next
  else
    let high = utf16_unit prepared index in
    if high < 0xD800 || high > 0xDBFF then index + 1
    else
      let low = utf16_unit prepared (index + 1) in
      if low >= 0xDC00 && low <= 0xDFFF then index + 2 else index + 1

let prepared_start_index prepared regexp =
  let index = if global regexp || sticky regexp then regexp.last_index else 0 in
  if
    prepared.buffer_type = 1 && index > 0
    && index < prepared.matching_length
    && unicode regexp
  then
    let current = utf16_unit prepared index in
    let previous = utf16_unit prepared (index - 1) in
    if
      current >= 0xDC00 && current <= 0xDFFF && previous >= 0xD800
      && previous <= 0xDBFF
    then index - 1
    else index
  else index

(* exec_prepared is not a direct binding to lre_exec but an implementation of
   js_regexp_exec from quickjs.c. *)
let exec_prepared ?timeout_ms regexp prepared =
  let input = prepared.input in
  let capture_count = Libregexp.get_capture_count regexp.bc in
  let capture_size = capture_count * 2 in
  let capture = Ctypes.CArray.make (Ctypes.ptr Ctypes.uint8_t) capture_size in
  let start_capture = Ctypes.CArray.start capture in

  let buffer = Ctypes.CArray.start prepared.buffer_storage in

  let start_index = prepared_start_index prepared regexp in

  if start_index > prepared.matching_length then begin
    (* No match possible: reset last_index like JavaScript does *)
    if global regexp || sticky regexp then regexp.last_index <- 0;
    None
  end
  else begin
    let deadline =
      match timeout_ms with
      | None -> None
      | Some ms ->
          Some (Ctypes.allocate Ctypes.double (Libregexp.now_ms () +. ms))
    in
    let exec_result =
      Libregexp.exec start_capture regexp.bc buffer start_index
        prepared.matching_length prepared.buffer_type deadline
    in
    match exec_result with
    | 1 ->
        (* Convert byte offsets in the matching buffer to UTF-16 code-unit
           indices. *)
        let to_utf16_index raw_bytes =
          match prepared.utf16_map with
          | Some _ -> raw_bytes / 2
          | None -> raw_bytes
        in
        (* UTF-16 [start, end) per group; None = did not participate.
           Entry 0 is the full match. *)
        let ranges =
          Array.init capture_count (fun group ->
              let start_ptr = Ctypes.CArray.get capture (group * 2) in
              let end_ptr = Ctypes.CArray.get capture ((group * 2) + 1) in
              if Ctypes.is_null start_ptr || Ctypes.is_null end_ptr then None
              else
                let u16_start =
                  to_utf16_index (Ctypes.ptr_diff buffer start_ptr)
                in
                let u16_end = to_utf16_index (Ctypes.ptr_diff buffer end_ptr) in
                Some (u16_start, u16_end))
        in
        let captures =
          Array.map
            (function
              | None -> None
              | Some (u16_start, u16_end) ->
                  Some
                    (prepared_substring prepared ~start:u16_start ~end_:u16_end))
            ranges
        in
        let match_start, match_end =
          (* The full match always participates when exec reports a match *)
          match ranges.(0) with
          | Some range -> range
          | None -> assert false
        in
        (* Only global/sticky regexps advance lastIndex, and only based on
           the full match (group 0) *)
        if global regexp || sticky regexp then regexp.last_index <- match_end;
        (* (name, group number) pairs in source order *)
        let named_groups =
          match Libregexp.get_groupnames regexp.bc with
          | None -> []
          | Some first_name ->
              (* The buffer holds one NUL-terminated entry per capture group
                 1..n; unnamed groups have an empty entry. *)
              let rec walk ptr group acc =
                if group >= capture_count then List.rev acc
                else
                  let name_len = strlen ptr in
                  let acc =
                    if name_len > 0 then
                      let name = Ctypes.string_from_ptr ~length:name_len ptr in
                      (name, group) :: acc
                    else acc
                  in
                  walk (Ctypes.( +@ ) ptr (name_len + 1)) (group + 1) acc
              in
              walk first_name 1 []
        in
        let groups =
          List.map (fun (name, group) -> (name, captures.(group))) named_groups
        in
        let indices =
          (* Like JavaScript's hasIndices: only populated with the 'd' flag *)
          if has_flag regexp.flags lre_flag_indices then
            Some
              {
                ranges;
                groups =
                  List.map
                    (fun (name, group) -> (name, ranges.(group)))
                    named_groups;
              }
          else None
        in
        let result =
          { captures; index = match_start; input; groups; indices }
        in
        let range =
          {
            utf16 = (match_start, match_end);
            bytes =
              prepared_byte_range prepared ~start:match_start ~end_:match_end;
          }
        in
        Some { result; range }
    | 0 ->
        (* No match: lastIndex resets for global/sticky regexps *)
        if global regexp || sticky regexp then regexp.last_index <- 0;
        None
    | -2 (* LRE_RET_TIMEOUT *) -> raise Timeout
    | _ (* -1, LRE_RET_MEMORY_ERROR *) -> raise Out_of_memory
  end

let exec ?timeout_ms regexp input =
  match exec_prepared ?timeout_ms regexp (prepare_input input) with
  | Some prepared_match -> Some prepared_match.result
  | None -> None

let test ?timeout_ms regexp input =
  match exec ?timeout_ms regexp input with Some _ -> true | None -> false
