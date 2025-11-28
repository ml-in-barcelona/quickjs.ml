type t = {
  bc : Unsigned.uint8 Ctypes_static.ptr;
  source : string;
  flags : int;
  mutable lastIndex : int;
}

type matchResult = {
  captures : string array;
  input : string;
  index : int;
  groups : (string * string) list;
}

type result = (matchResult, string) Stdlib.result

(* #define LRE_FLAG_GLOBAL (1 << 0) *)
let lre_flag_global = 0b01

(* #define LRE_FLAG_IGNORECASE (1 << 1) *)
let lre_flag_ignorecase = 0b10

(* #define LRE_FLAG_MULTILINE (1 << 2) *)
let lre_flag_multiline = 0b100

(* #define LRE_FLAG_DOTALL (1 << 3) *)
let lre_flag_dotall = 0b1000

(* #define LRE_FLAG_UNICODE (1 << 4) *)
let lre_flag_unicode = 0b10000

(* #define LRE_FLAG_STICKY (1 << 5) *)
let lre_flag_sticky = 0b100000
let has_flag flags flag = flags land flag != 0
let global regexp = has_flag regexp.flags lre_flag_global
let ignorecase regexp = has_flag regexp.flags lre_flag_ignorecase
let multiline regexp = has_flag regexp.flags lre_flag_multiline
let dotall regexp = has_flag regexp.flags lre_flag_dotall
let sticky regexp = has_flag regexp.flags lre_flag_sticky
let unicode regexp = has_flag regexp.flags lre_flag_unicode

let parse_flags flags =
  let rec parse_flags' flags acc =
    match flags with
    | [] -> acc
    | 'g' :: rest -> parse_flags' rest (acc lor lre_flag_global)
    | 'i' :: rest -> parse_flags' rest (acc lor lre_flag_ignorecase)
    | 'm' :: rest -> parse_flags' rest (acc lor lre_flag_multiline)
    | 's' :: rest -> parse_flags' rest (acc lor lre_flag_dotall)
    | 'u' :: rest -> parse_flags' rest (acc lor lre_flag_unicode)
    | 'y' :: rest -> parse_flags' rest (acc lor lre_flag_sticky)
    | _ :: rest -> parse_flags' rest acc
  in
  parse_flags' (String.to_seq flags |> List.of_seq) 0

let flags_to_string flags =
  let rec flags_to_string' flags acc =
    match flags with
    | 0 -> acc
    | _ when has_flag flags lre_flag_global ->
        flags_to_string' (flags land lre_flag_global lxor flags) (acc ^ "g")
    | _ when has_flag flags lre_flag_ignorecase ->
        flags_to_string' (flags land lre_flag_ignorecase lxor flags) (acc ^ "i")
    | _ when has_flag flags lre_flag_multiline ->
        flags_to_string' (flags land lre_flag_multiline lxor flags) (acc ^ "m")
    | _ when has_flag flags lre_flag_dotall ->
        flags_to_string' (flags land lre_flag_dotall lxor flags) (acc ^ "s")
    | _ when has_flag flags lre_flag_unicode ->
        flags_to_string' (flags land lre_flag_unicode lxor flags) (acc ^ "u")
    | _ when has_flag flags lre_flag_sticky ->
        flags_to_string' (flags land lre_flag_sticky lxor flags) (acc ^ "y")
    | _ -> acc
  in
  flags_to_string' flags ""

let strlen ptr =
  let rec aux ptr len =
    let c = Ctypes.( !@ ) ptr in
    if c = char_of_int 0 then len else aux (Ctypes.( +@ ) ptr 1) (len + 1)
  in
  aux ptr 0

(* Check if a string contains non-ASCII bytes that require Unicode mode in libregexp.
   Any byte >= 0x80 indicates multi-byte UTF-8 which needs Unicode mode for proper matching. *)
let needs_unicode_mode s =
  let len = String.length s in
  let rec check i =
    if i >= len then false
    else
      let byte = Char.code s.[i] in
      if byte >= 0x80 then true
      else check (i + 1)
  in
  check 0

let compile ~flags re =
  let compiled_byte_code_len = Ctypes.allocate Ctypes.int 0 in
  let size_of_error_msg = 64 in
  let error_msg = Ctypes.allocate_n ~count:size_of_error_msg Ctypes.char in
  let input = Ctypes.ocaml_string_start re in
  let input_length = String.length re |> Unsigned.Size_t.of_int in
  let parsed_flags = parse_flags flags in
  (* Auto-enable Unicode mode for patterns containing 4-byte UTF-8 sequences
     (code points >= U+10000, like emojis). libregexp requires this. *)
  let flags =
    if needs_unicode_mode re then parsed_flags lor lre_flag_unicode
    else parsed_flags
  in
  let compiled_byte_code =
    Bindings.C.Functions.lre_compile compiled_byte_code_len error_msg
      size_of_error_msg input input_length flags Ctypes.null
  in
  match Ctypes.is_null compiled_byte_code with
  | false -> Ok { bc = compiled_byte_code; flags; lastIndex = 0; source = re }
  | true ->
      let length = strlen error_msg in
      let error = Ctypes.string_from_ptr ~length error_msg in
      Error
        ( (match error with
          | "unexpected end" -> `Unexpected_end
          | "malformed unicode char" -> `Malformed_unicode_char
          | "nothing to repeat" -> `Nothing_to_repeat
          | "invalid escape sequence in regular expression" ->
              `Invalid_escape_sequence
          | unknown -> `Unknown unknown),
          error )

let index result = match result with Ok result -> result.index | Error _ -> 0
let lastIndex regexp = regexp.lastIndex
let source regexp = regexp.source
let input result = match result with Ok result -> result.input | Error _ -> ""
let setLastIndex regexp lastIndex = regexp.lastIndex <- lastIndex

let captures result =
  match result with Ok result -> result.captures | Error _ -> [||]

let groups result =
  match result with Ok result -> result.groups | Error _ -> []

let group name result =
  match result with
  | Ok result -> List.assoc_opt name result.groups
  | Error _ -> None

let flags regexp = flags_to_string regexp.flags

(* Convert UTF-8 string to UTF-16 code units (as uint8_t pairs, little-endian) *)
let utf8_to_utf16_bytes s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s * 2) in
  let add_u16 code =
    Buffer.add_char buf (Char.chr (code land 0xFF));
    Buffer.add_char buf (Char.chr ((code lsr 8) land 0xFF))
  in
  let rec loop () =
    match Uutf.decode decoder with
    | `Uchar u ->
        let code = Uchar.to_int u in
        if code < 0x10000 then add_u16 code
        else begin
          (* Surrogate pair for code points >= 0x10000 *)
          let code' = code - 0x10000 in
          add_u16 (0xD800 lor (code' lsr 10));
          add_u16 (0xDC00 lor (code' land 0x3FF))
        end;
        loop ()
    | `End -> Buffer.contents buf
    | `Malformed _ ->
        add_u16 0xFFFD; (* Replacement character *)
        loop ()
    | `Await -> assert false
  in
  loop ()

(* Build a mapping from UTF-16 code unit index to UTF-8 byte index *)
let build_utf16_to_utf8_map s =
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let map = ref [] in
  let utf16_idx = ref 0 in
  let rec loop () =
    let byte_idx = Uutf.decoder_byte_count decoder in
    match Uutf.decode decoder with
    | `Uchar u ->
        map := (!utf16_idx, byte_idx) :: !map;
        let code = Uchar.to_int u in
        if code < 0x10000 then incr utf16_idx
        else utf16_idx := !utf16_idx + 2; (* Surrogate pair *)
        loop ()
    | `End ->
        map := (!utf16_idx, byte_idx) :: !map;
        Array.of_list (List.rev !map)
    | `Malformed _ ->
        map := (!utf16_idx, byte_idx) :: !map;
        incr utf16_idx;
        loop ()
    | `Await -> assert false
  in
  loop ()

(* Convert UTF-16 index to UTF-8 byte index using the map *)
let utf16_to_utf8_index map utf16_idx =
  (* Binary search or linear scan *)
  let rec find i =
    if i >= Array.length map then
      snd map.(Array.length map - 1)
    else
      let (u16, u8) = map.(i) in
      if u16 = utf16_idx then u8
      else if u16 > utf16_idx then
        if i = 0 then 0 else snd map.(i - 1)
      else find (i + 1)
  in
  find 0

(* exec is not a binding to lre_exec but an implementation of `js_regexp_exec` *)
let exec regexp input =
  let capture_count = Bindings.C.Functions.lre_get_capture_count regexp.bc in
  let capture_size = capture_count * 2 in
  let capture = Ctypes.CArray.make (Ctypes.ptr Ctypes.uint8_t) capture_size in
  let start_capture = Ctypes.CArray.start capture in

  (* Check if we need Unicode mode (pattern has non-ASCII or unicode flag) *)
  let use_unicode = unicode regexp || needs_unicode_mode regexp.source ||
                    needs_unicode_mode input in

  let buffer, matching_length, shift, utf16_map =
    if use_unicode then begin
      (* Convert UTF-8 input to UTF-16 for proper Unicode matching *)
      let utf16_str = utf8_to_utf16_bytes input in
      let utf16_len = String.length utf16_str in
      let bytes_list =
        List.init utf16_len (fun i ->
            Unsigned.UInt8.of_int (Char.code utf16_str.[i]))
      in
      let bufp = Ctypes.CArray.of_list Ctypes.uint8_t bytes_list in
      let map = build_utf16_to_utf8_map input in
      (Ctypes.CArray.start bufp, utf16_len / 2, 1, Some map)
    end else begin
      (* ASCII-only: use bytes directly *)
      let bytes_list =
        List.init (String.length input) (fun i ->
            Unsigned.UInt8.of_int (Char.code input.[i]))
      in
      let bufp = Ctypes.CArray.of_list Ctypes.uint8_t bytes_list in
      (Ctypes.CArray.start bufp, String.length input, 0, None)
    end
  in

  let lastIndex =
    match global regexp || sticky regexp with
    | true -> regexp.lastIndex
    | false -> 0
  in

  let exec_result =
    Bindings.C.Functions.lre_exec start_capture regexp.bc buffer lastIndex
      matching_length shift Ctypes.null
  in

  match exec_result with
  | 1 ->
      let substrings = Array.make capture_count "" in
      let i = ref 0 in
      let index = ref 0 in
      let groups = ref [] in
      let group_name_ptr =
        ref (Bindings.C.Functions.lre_get_groupnames regexp.bc)
      in
      while !i < capture_size - 1 do
        let start_ptr = Ctypes.CArray.get capture !i in
        let end_ptr = Ctypes.CArray.get capture (!i + 1) in
        let raw_start = Ctypes.ptr_diff buffer start_ptr in
        let raw_length = Ctypes.ptr_diff start_ptr end_ptr in

        (* Convert indices based on mode *)
        let start_index, length =
          match utf16_map with
          | Some map ->
              (* UTF-16 mode: convert indices back to UTF-8 byte positions *)
              let u16_start = raw_start / 2 in
              let u16_end = u16_start + (raw_length / 2) in
              let u8_start = utf16_to_utf8_index map u16_start in
              let u8_end = utf16_to_utf8_index map u16_end in
              (u8_start, u8_end - u8_start)
          | None ->
              (* ASCII mode: indices are byte positions *)
              (raw_start, raw_length)
        in

        (* Only set index on first capture (the full match) *)
        if !i = 0 then index := start_index;
        let substring =
          match String.sub input start_index length with
          | sub -> sub
          | exception _ -> ""
        in
        (* Store the captured substring *)
        substrings.(!i / 2) <- substring;
        (* Update the lastIndex *)
        regexp.lastIndex <- start_index + length;

        (* if (\*group_name_ptr) { *)
        (match !group_name_ptr with
        (* if (group_name_ptr && i > 0) { *)
        | Some pointer when !i > 0 ->
            (*
              if (JS_DefinePropertyValueStr(ctx, groups, group_name_ptr, JS_DupValue(ctx, val), prop_flags) < 0) {
                  JS_FreeValue(ctx, val);
                  goto fail;
              }
            *)
            (* store the group name and its captured value, but only if named *)
            let name_len = strlen pointer in
            (if name_len > 0 then
               let current_group_name =
                 Ctypes.string_from_ptr ~length:name_len pointer
               in
               groups := (current_group_name, substring) :: !groups);
            (* group_name_ptr += strlen(group_name_ptr) + 1; *)
            let next_group_name_ptr = Ctypes.( +@ ) pointer (name_len + 1) in
            if Ctypes.is_null next_group_name_ptr then group_name_ptr := None
            else group_name_ptr := Some next_group_name_ptr
        | None | Some _ -> ());
        (* Incement the index *)
        i := !i + 2
      done;
      Ok { captures = substrings; input; index = !index; groups = !groups }
  | 0 ->
      (* When there's no matches left, lastIndex goes back to 0 *)
      (match sticky regexp || global regexp with
      | true -> regexp.lastIndex <- 0
      | false -> ());
      Ok { captures = [||]; input; index = 0; groups = [] }
  | _ (* -1 *) -> Error "Error"

let test regexp input =
  let result = exec regexp input in
  match result with
  | Ok result -> Array.length result.captures > 0
  | Error _ -> false
