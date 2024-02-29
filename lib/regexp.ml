(*
   (* The RegExp object *)

let captures : result -> string nullable array =
  fun result -> Pcre.get_opt_substrings result.substrings

let matches : result -> string array =
  fun result -> Pcre.get_substrings result.substrings

let index : result -> int =
  fun result ->
  try
    let substring = result.substrings in
    let start_offset, _end_offset = Pcre.get_substring_ofs substring 0 in
    start_offset
  with Not_found -> 0

let input : result -> string =
  fun result -> Pcre.get_subject result.substrings

let source : t -> string = fun _ -> notImplemented "Js.Re" "source"

let fromString : string -> t =
  fun str ->
  try
    let regexp = Pcre.regexp str in
    { regex = regexp; flags = []; lastIndex = 0 }
  with
  | Pcre.Error BadPartial -> raise @@ Invalid_argument "BadPartial"
  | Pcre.Error (BadPattern (msg, _pos)) ->
      raise @@ Invalid_argument ("BadPattern: " ^ msg)
  | Pcre.Error Partial -> raise @@ Invalid_argument "Partial"
  | Pcre.Error BadUTF8 -> raise @@ Invalid_argument "BadUTF8"
  | Pcre.Error BadUTF8Offset -> raise @@ Invalid_argument "BadUTF8Offset"
  | Pcre.Error MatchLimit -> raise @@ Invalid_argument "MatchLimit"
  | Pcre.Error RecursionLimit -> raise @@ Invalid_argument "RecursionLimit"
  | Pcre.Error WorkspaceSize -> raise @@ Invalid_argument "WorkspaceSize"
  | Pcre.Error (InternalError msg) -> raise @@ Invalid_argument msg

let fromStringWithFlags : string -> flags:string -> t =
  fun str ~flags:str_flags ->
  let flags = parse_flags str_flags in
  let pcre_flags = List.filter_map cflag_of_flag flags in
  let regexp = Pcre.regexp ~flags:pcre_flags str in
  { regex = regexp; flags = parse_flags str_flags; lastIndex = 0 }

let flags : t -> string =
  fun regexp ->
  let options = Pcre.options regexp.regex in
  let flags = Pcre.cflag_list options in
  flags |> List.filter_map char_of_cflag |> List.to_seq |> String.of_seq

let flag : t -> flag -> bool = fun regexp flag -> List.mem flag regexp.flags
let global : t -> bool = fun regexp -> flag regexp `GLOBAL
let ignoreCase : t -> bool = fun regexp -> flag regexp `CASELESS
let multiline : t -> bool = fun regexp -> flag regexp `MULTILINE
let sticky : t -> bool = fun regexp -> flag regexp `STICKY
let unicode : t -> bool = fun regexp -> flag regexp `UNICODE
let lastIndex : t -> int = fun regex -> regex.lastIndex

let setLastIndex : t -> int -> unit =
  fun regex index -> regex.lastIndex <- index

let exec_ : t -> string -> result option =
  fun regexp str ->
  try
    let rex = regexp.regex in
    let substrings = Pcre.exec ~rex ~pos:regexp.lastIndex str in
    let _, lastIndex = Pcre.get_substring_ofs substrings 0 in
    regexp.lastIndex <- lastIndex;
    Some { substrings }
  with Not_found -> None

let exec : string -> t -> result option = fun str rex -> exec_ rex str

let test_ : t -> string -> bool =
  fun regexp str -> Pcre.pmatch ~rex:regexp.regex str
*)

[@@@warning "-69"]

type flags = {
  global : bool;
  ignore_case : bool;
  multiline : bool;
  sticky : bool;
  unicode : bool;
  unicode_sets : bool;
  dot_all : bool;
}

(* #define LRE_FLAG_GLOBAL     (1 << 0)
   #define LRE_FLAG_IGNORECASE (1 << 1)
   #define LRE_FLAG_MULTILINE  (1 << 2)
   #define LRE_FLAG_DOTALL     (1 << 3)
   #define LRE_FLAG_UNICODE    (1 << 4)
   #define LRE_FLAG_STICKY     (1 << 5)
   #define LRE_FLAG_INDICES    (1 << 6) /* Unused by libregexp, just recorded. */
   #define LRE_FLAG_NAMED_GROUPS (1 << 7) /* named groups are present in the regexp */ *)

type regex = { bc : Unsigned.uint8 Ctypes_static.ptr; flags : flags }
type result

let parse_flags flags =
  let rec parse_flags' flags acc =
    match flags with
    | [] -> acc
    | 'g' :: rest -> parse_flags' rest { acc with global = true }
    | 'i' :: rest -> parse_flags' rest { acc with ignore_case = true }
    | 'm' :: rest -> parse_flags' rest { acc with multiline = true }
    | 's' :: rest -> parse_flags' rest { acc with dot_all = true }
    | 'u' :: rest -> parse_flags' rest { acc with unicode = true }
    | 'y' :: rest -> parse_flags' rest { acc with sticky = true }
    | _ :: rest -> parse_flags' rest acc
  in
  parse_flags'
    (String.to_seq flags |> List.of_seq)
    {
      global = false;
      ignore_case = false;
      multiline = false;
      sticky = false;
      unicode = false;
      unicode_sets = false;
      dot_all = false;
    }

let compile re =
  let compiled_byte_code_len = Ctypes.allocate Ctypes.int 0 in
  let size_of_error_msg = 64 in
  let error_msg = Ctypes.allocate_n ~count:size_of_error_msg Ctypes.char in
  let input = Ctypes.ocaml_string_start re in
  let input_length = String.length re |> Unsigned.Size_t.of_int in
  let flags = 0 in
  let compiled_byte_code =
    Libregexp.C.Functions.lre_compile compiled_byte_code_len error_msg
      size_of_error_msg input input_length flags Ctypes.null
  in
  match Ctypes.is_null compiled_byte_code with
  | true ->
      let error = Ctypes.string_from_ptr ~length:64 error_msg in
      print_endline error;
      raise (Invalid_argument "Compilation failed")
  | false ->
      {
        bc = compiled_byte_code;
        flags =
          {
            global = false;
            ignore_case = false;
            multiline = false;
            sticky = false;
            unicode = false;
            unicode_sets = false;
            dot_all = false;
          };
      }

let exec input regexp =
  let { bc; _ } = regexp in
  let capture_count = Libregexp.C.Functions.lre_get_capture_count bc in
  let capture = Ctypes.CArray.make Ctypes.uint8_t capture_count in
  let start = Ctypes.CArray.start capture in
  let start_capture = Ctypes.allocate (Ctypes.ptr Ctypes.uint8_t) start in
  let matching_length = String.length input in
  let _matching = Ctypes.ocaml_string_start input in
  let bufp =
    Ctypes.CArray.of_list Ctypes.char (input |> String.to_seq |> List.of_seq)
  in
  let buffer =
    Ctypes.coerce (Ctypes.ptr Ctypes.char)
      (Ctypes.ptr Ctypes.uint8_t)
      (Ctypes.CArray.start bufp)
  in
  Printf.printf "\nmatching_length %d\n" matching_length;
  let index = 0 in
  (* Return 1 if match, 0 if not match or -1 if error. cindex is the
     starting position of the match and must be such as 0 <= cindex <=
     clen. *)
  let exec_result =
    Libregexp.C.Functions.lre_exec start_capture bc buffer index matching_length
      0 Ctypes.null
  in
  Printf.printf "\ncapture_count %d\n" capture_count;
  match exec_result with
  | 1 ->
      capture
      |> Ctypes.CArray.iter (fun i ->
             Printf.sprintf "capture: %d" (Unsigned.UInt8.to_int i)
             |> print_endline);
      (* printd_intCtypes.CArray.length capture *)
      [||]
  | 0 ->
      Printf.sprintf "nothing found" |> print_endline;
      [||]
  | _ (* -1 *) -> raise (Invalid_argument "Error")

(* static JSValue js_regexp_exec(
       JSContext *ctx,
       JSValueConst this_val,
       int argc,
       JSValueConst *argv) {
       JSRegExp *re = js_get_regexp(ctx, this_val, TRUE);
       JSString *str;
       JSValue str_val, obj, val, groups = JS_UNDEFINED;
       uint8_t *re_bytecode;
       int ret;
       uint8_t **capture, *str_buf;
       int capture_count, shift, i, re_flags;
       int64_t last_index;
       const char *group_name_ptr;

       if (!re)
           return JS_EXCEPTION;
       str_val = JS_ToString(ctx, argv[0]);
       if (JS_IsException(str_val))
           return str_val;
       val = JS_GetProperty(ctx, this_val, JS_ATOM_lastIndex);
       if (JS_IsException(val) ||
           JS_ToLengthFree(ctx, &last_index, val)) {
           JS_FreeValue(ctx, str_val);
           return JS_EXCEPTION;
       }
       re_bytecode = re->bytecode->u.str8;
       re_flags = lre_get_flags(re_bytecode);
       if ((re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) == 0) {
           last_index = 0;
       }
       str = JS_VALUE_GET_STRING(str_val);
       capture_count = lre_get_capture_count(re_bytecode);
       capture = NULL;
       if (capture_count > 0) {
           capture = js_malloc(ctx, sizeof(capture[0]) * capture_count * 2);
           if (!capture) {
               JS_FreeValue(ctx, str_val);
               return JS_EXCEPTION;
           }
       }
       shift = str->is_wide_char;
       str_buf = str->u.str8;
       if (last_index > str->len) {
           ret = 2;
       } else {
           ret = lre_exec(capture, re_bytecode,
                          str_buf, last_index, str->len,
                          shift, ctx);
       }
       obj = JS_NULL;
       if (ret != 1) {
           if (ret >= 0) {
               if (ret == 2 || (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY))) {
                   if (JS_SetProperty(ctx, this_val, JS_ATOM_lastIndex,
                                      JS_NewInt32(ctx, 0)) < 0)
                       goto fail;
               }
           } else {
               JS_ThrowInternalError(ctx, "out of memory in regexp execution");
               goto fail;
           }
           JS_FreeValue(ctx, str_val);
       } else {
           int prop_flags;
           if (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) {
               if (JS_SetProperty(ctx, this_val, JS_ATOM_lastIndex,
                                  JS_NewInt32(ctx, (capture[1] - str_buf) >> shift)) < 0)
                   goto fail;
           }
           obj = JS_NewArray(ctx);
           if (JS_IsException(obj))
               goto fail;
           prop_flags = JS_PROP_C_W_E | JS_PROP_THROW;
           group_name_ptr = lre_get_groupnames(re_bytecode);
           if (group_name_ptr) {
               groups = JS_NewObjectProto(ctx, JS_NULL);
               if (JS_IsException(groups))
                   goto fail;
           }

           for(i = 0; i < capture_count; i++) {
               int start, end;
               JSValue val;
               if (capture[2 * i] == NULL ||
                   capture[2 * i + 1] == NULL) {
                   val = JS_UNDEFINED;
               } else {
                   start = (capture[2 * i] - str_buf) >> shift;
                   end = (capture[2 * i + 1] - str_buf) >> shift;
                   val = js_sub_string(ctx, str, start, end);
                   if (JS_IsException(val))
                       goto fail;
               }
               if (group_name_ptr && i > 0) {
                   if (*group_name_ptr) {
                       if (JS_DefinePropertyValueStr(ctx, groups, group_name_ptr,
                                                     JS_DupValue(ctx, val),
                                                     prop_flags) < 0) {
                           JS_FreeValue(ctx, val);
                           goto fail;
                       }
                   }
                   group_name_ptr += strlen(group_name_ptr) + 1;
               }
               if (JS_DefinePropertyValueUint32(ctx, obj, i, val, prop_flags) < 0)
                   goto fail;
           }
           if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_groups,
                                      groups, prop_flags) < 0)
               goto fail;
           if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_index,
                                      JS_NewInt32(ctx, (capture[0] - str_buf) >> shift), prop_flags) < 0)
               goto fail;
           if (JS_DefinePropertyValue(ctx, obj, JS_ATOM_input, str_val, prop_flags) < 0)
               goto fail1;
       }
       js_free(ctx, capture);
       return obj;
   fail:
       JS_FreeValue(ctx, groups);
       JS_FreeValue(ctx, str_val);
   fail1:
       JS_FreeValue(ctx, obj);
       js_free(ctx, capture);
       return JS_EXCEPTION;
   }

   *)
*)
