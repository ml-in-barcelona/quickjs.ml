(** JavaScript String built-in object

    This module mirrors the JavaScript String API with prototype methods for
    string manipulation. All methods use UTF-16 semantics for indices.

    {2 UTF-8 Validation}

    This module expects valid UTF-8 input. When malformed UTF-8 is encountered:
    - Invalid byte sequences are replaced with U+FFFD (replacement character)
    - The replacement preserves iteration behavior (each invalid sequence = 1
      replacement)
    - This matches JavaScript's behavior when handling invalid string data

    Use [is_valid_utf8] to check strings before processing if strict validation
    is required. *)

module Libunicode = Quickjs_c.Libunicode

(* Re-export normalization type from Unicode *)
type normalization = Unicode.normalization = NFC | NFD | NFKC | NFKD

(** Check if a string is valid UTF-8. Returns true if all byte sequences are
    valid UTF-8 encoded characters. *)
let is_valid_utf8 s =
  let len = Stdlib.String.length s in
  let rec check i =
    if i >= len then true
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      if Uchar.utf_decode_is_valid d then check (i + Uchar.utf_decode_length d)
      else false
  in
  check 0

(* UTF-16 helper functions *)

(** Get the UTF-16 length of a string (number of code units). Uses
    tail-recursive iteration for stack safety. *)
let utf16_length s =
  let len = Stdlib.String.length s in
  let rec loop i count =
    if i >= len then count
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let code = Uchar.to_int u in
      let units = if code >= 0x10000 then 2 else 1 in
      loop (i + Uchar.utf_decode_length d) (count + units)
  in
  loop 0 0

(** Result of looking up a UTF-16 index in a UTF-8 string *)
type utf16_lookup_result =
  | Out_of_bounds
  | Single_unit of int (* BMP character: code point *)
  | High_surrogate of int * int (* Surrogate pair: high unit, full code point *)
  | Low_surrogate of int (* Low surrogate: low unit *)

(** Look up a single UTF-16 code unit at the given index. This is O(n) where n
    is the index, but avoids allocating the full array. For repeated access at
    nearby indices, use to_utf16_array instead. *)
let utf16_at idx s =
  if idx < 0 then Out_of_bounds
  else
    let len = Stdlib.String.length s in
    let rec loop i u16_idx =
      if i >= len then Out_of_bounds
      else
        let d = Stdlib.String.get_utf_8_uchar s i in
        let u = Uchar.utf_decode_uchar d in
        let code = Uchar.to_int u in
        if code >= 0x10000 then begin
          (* This character takes 2 UTF-16 code units (surrogate pair) *)
          if u16_idx = idx then
            (* Requesting high surrogate *)
            let code' = code - 0x10000 in
            let high = 0xD800 lor (code' lsr 10) in
            High_surrogate (high, code)
          else if u16_idx + 1 = idx then
            (* Requesting low surrogate *)
            let code' = code - 0x10000 in
            let low = 0xDC00 lor (code' land 0x3FF) in
            Low_surrogate low
          else loop (i + Uchar.utf_decode_length d) (u16_idx + 2)
        end
        else begin
          (* This character takes 1 UTF-16 code unit *)
          if u16_idx = idx then Single_unit code
          else loop (i + Uchar.utf_decode_length d) (u16_idx + 1)
        end
    in
    loop 0 0

(** Convert UTF-8 string to array of UTF-16 code units. Two-pass approach: first
    count, then fill. Tail-recursive and stack-safe. *)
let to_utf16_array s =
  let byte_len = Stdlib.String.length s in
  (* First pass: count UTF-16 code units *)
  let u16_len = utf16_length s in
  if u16_len = 0 then [||]
  else begin
    let arr = Array.make u16_len 0 in
    (* Second pass: fill the array *)
    let rec fill byte_idx u16_idx =
      if byte_idx >= byte_len then ()
      else
        let d = Stdlib.String.get_utf_8_uchar s byte_idx in
        let u = Uchar.utf_decode_uchar d in
        let code = Uchar.to_int u in
        if code >= 0x10000 then begin
          (* Surrogate pair *)
          let code' = code - 0x10000 in
          arr.(u16_idx) <- 0xD800 lor (code' lsr 10);
          arr.(u16_idx + 1) <- 0xDC00 lor (code' land 0x3FF);
          fill (byte_idx + Uchar.utf_decode_length d) (u16_idx + 2)
        end
        else begin
          arr.(u16_idx) <- code;
          fill (byte_idx + Uchar.utf_decode_length d) (u16_idx + 1)
        end
    in
    fill 0 0;
    arr
  end

(** Check if a string contains only ASCII characters (bytes < 128). For
    ASCII-only strings, byte indices equal UTF-16 indices. *)
let is_ascii s =
  let len = Stdlib.String.length s in
  let rec check i =
    if i >= len then true
    else if Char.code (Stdlib.String.get s i) >= 128 then false
    else check (i + 1)
  in
  check 0

(** JavaScript whitespace (ECMA-262 WhiteSpace or LineTerminator). Delegates to
    libunicode's lre_is_space, the same predicate QuickJS uses for
    String.prototype.trim, so there is a single source of truth. *)
let is_whitespace code = Libunicode.is_space code

(* Index unit conversions between UTF-8 byte offsets and UTF-16 code unit
   indices. Useful at the boundary with byte-oriented code (e.g. consumers
   slicing with Stdlib.String.sub around RegExp match indices). *)

let utf16_index_of_byte s byte_index =
  let len = Stdlib.String.length s in
  let rec loop i u16 =
    if i >= byte_index || i >= len then u16
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
      let units = if code >= 0x10000 then 2 else 1 in
      loop (i + Uchar.utf_decode_length d) (u16 + units)
  in
  loop 0 0

let byte_index_of_utf16 s utf16_index =
  let len = Stdlib.String.length s in
  let rec loop i u16 =
    if u16 >= utf16_index || i >= len then i
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let code = Uchar.to_int (Uchar.utf_decode_uchar d) in
      let units = if code >= 0x10000 then 2 else 1 in
      loop (i + Uchar.utf_decode_length d) (u16 + units)
  in
  loop 0 0

(** Convert array of UTF-16 code units back to UTF-8 string. Tail-recursive for
    stack safety. Surrogate pairs combine into their code point; unpaired
    surrogates become U+FFFD since UTF-8 cannot represent them. *)
let from_utf16_array arr =
  let len = Array.length arr in
  if len = 0 then ""
  else begin
    let buf = Stdlib.Buffer.create (len * 3) in
    let rec loop i =
      if i >= len then Stdlib.Buffer.contents buf
      else
        let code = arr.(i) in
        if code >= 0xD800 && code <= 0xDBFF then begin
          (* High surrogate: pairs with a following low surrogate *)
          let low = if i + 1 < len then arr.(i + 1) else 0 in
          if low >= 0xDC00 && low <= 0xDFFF then begin
            let cp = 0x10000 + ((code - 0xD800) * 0x400) + (low - 0xDC00) in
            Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
            loop (i + 2)
          end
          else begin
            (* Lone high surrogate - output replacement character *)
            Stdlib.Buffer.add_utf_8_uchar buf Uchar.rep;
            loop (i + 1)
          end
        end
        else if code >= 0xDC00 && code <= 0xDFFF then begin
          (* Lone low surrogate - output replacement character *)
          Stdlib.Buffer.add_utf_8_uchar buf Uchar.rep;
          loop (i + 1)
        end
        else begin
          let u =
            if code >= 0 && code <= 0x10FFFF then Uchar.of_int code
            else Uchar.rep
          in
          Stdlib.Buffer.add_utf_8_uchar buf u;
          loop (i + 1)
        end
    in
    loop 0
  end

let from_char_code codes =
  (* land 0xFFFF is ToUint16 *)
  from_utf16_array (Array.map (fun code -> code land 0xFFFF) codes)

(* Expanding to UTF-16 code units first makes adjacent surrogate halves pair
   up, like in JavaScript. *)
let from_code_point code_points =
  Array.iter
    (fun cp ->
      if cp < 0 || cp > 0x10FFFF then
        (* RangeError in JavaScript *)
        invalid_arg
          (Printf.sprintf "String.from_code_point: invalid code point %d" cp))
    code_points;
  let unit_count =
    Array.fold_left
      (fun count cp -> count + if cp >= 0x10000 then 2 else 1)
      0 code_points
  in
  let units = Array.make unit_count 0 in
  let idx = ref 0 in
  Array.iter
    (fun cp ->
      if cp >= 0x10000 then begin
        let cp' = cp - 0x10000 in
        units.(!idx) <- 0xD800 lor (cp' lsr 10);
        units.(!idx + 1) <- 0xDC00 lor (cp' land 0x3FF);
        idx := !idx + 2
      end
      else begin
        units.(!idx) <- cp;
        incr idx
      end)
    code_points;
  from_utf16_array units

module Prototype = struct
  (** String.prototype methods *)

  let to_lower_case = Unicode.lowercase
  let to_upper_case = Unicode.uppercase
  let normalize = Unicode.normalize

  (** Get the UTF-16 length of a string *)
  let length = utf16_length

  (** Get character at UTF-16 index. Optimized to avoid full array conversion
      for single character access. Note: Accessing a lone surrogate returns the
      replacement character since UTF-8 cannot represent unpaired surrogates. *)
  let char_at idx s =
    match utf16_at idx s with
    | Out_of_bounds -> ""
    | Single_unit code ->
        let buf = Stdlib.Buffer.create 4 in
        Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_int code);
        Stdlib.Buffer.contents buf
    | High_surrogate (_, full_code) ->
        (* Return the full character for high surrogate position *)
        let buf = Stdlib.Buffer.create 4 in
        Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_int full_code);
        Stdlib.Buffer.contents buf
    | Low_surrogate _ ->
        (* Lone low surrogate - return replacement character (UTF-8 can't represent surrogates) *)
        let buf = Stdlib.Buffer.create 4 in
        Stdlib.Buffer.add_utf_8_uchar buf Uchar.rep;
        Stdlib.Buffer.contents buf

  (** Get UTF-16 code unit at index. Optimized to avoid full array conversion
      for single character access. *)
  let char_code_at idx s =
    match utf16_at idx s with
    | Out_of_bounds -> None
    | Single_unit code -> Some code
    | High_surrogate (high, _) -> Some high
    | Low_surrogate low -> Some low

  (** Get full Unicode code point at UTF-16 index. Optimized to avoid full array
      conversion for single character access. *)
  let code_point_at idx s =
    match utf16_at idx s with
    | Out_of_bounds -> None
    | Single_unit code -> Some code
    | High_surrogate (_, full_code) -> Some full_code
    | Low_surrogate low -> Some low

  (** Clamp value to range [min, max] *)
  let clamp v min max = if v < min then min else if v > max then max else v

  (** slice(start, end) - extract substring with negative index support *)
  let slice ~start ~end_ s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    let start = if start < 0 then max 0 (len + start) else min start len in
    let end_ = if end_ < 0 then max 0 (len + end_) else min end_ len in
    if start >= end_ then ""
    else from_utf16_array (Array.sub arr start (end_ - start))

  let slice_from start s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    let start = if start < 0 then max 0 (len + start) else min start len in
    from_utf16_array (Array.sub arr start (len - start))

  (** substring(start, end) - extract substring, swaps if start > end *)
  let substring ~start ~end_ s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    let start = clamp (max 0 start) 0 len in
    let end_ = clamp (max 0 end_) 0 len in
    let start, end_ = if start > end_ then (end_, start) else (start, end_) in
    from_utf16_array (Array.sub arr start (end_ - start))

  let substring_from start s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    let start = clamp (max 0 start) 0 len in
    from_utf16_array (Array.sub arr start (len - start))

  (** substr(start, length) - legacy method, extracts by length *)
  let substr ~start ~length s =
    if length <= 0 then ""
    else
      let arr = to_utf16_array s in
      let len = Array.length arr in
      let start = if start < 0 then max 0 (len + start) else start in
      if start >= len then ""
      else
        let actual_len = min length (len - start) in
        from_utf16_array (Array.sub arr start actual_len)

  let substr_from start s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    let start = if start < 0 then max 0 (len + start) else start in
    if start >= len then ""
    else from_utf16_array (Array.sub arr start (len - start))

  (** indexOf - find first occurrence. Optimized fast path for ASCII-only
      strings. *)
  let index_of search s =
    if Stdlib.String.length search = 0 then 0
    else if is_ascii s && is_ascii search then begin
      (* Fast path: for ASCII strings, byte indices = UTF-16 indices *)
      let s_len = Stdlib.String.length s in
      let search_len = Stdlib.String.length search in
      if search_len > s_len then -1
      else
        let rec find i =
          if i > s_len - search_len then -1
          else if Stdlib.String.sub s i search_len = search then i
          else find (i + 1)
        in
        find 0
    end
    else begin
      (* General path: convert to UTF-16 *)
      let s_arr = to_utf16_array s in
      let search_arr = to_utf16_array search in
      let s_len = Array.length s_arr in
      let search_len = Array.length search_arr in
      if search_len > s_len then -1
      else
        let found = ref (-1) in
        let i = ref 0 in
        while !found = -1 && !i <= s_len - search_len do
          let matches = ref true in
          let j = ref 0 in
          while !matches && !j < search_len do
            if s_arr.(!i + !j) <> search_arr.(!j) then matches := false;
            incr j
          done;
          if !matches then found := !i;
          incr i
        done;
        !found
    end

  let index_of_from search start s =
    if Stdlib.String.length search = 0 then
      let len = utf16_length s in
      clamp start 0 len
    else
      let s_arr = to_utf16_array s in
      let search_arr = to_utf16_array search in
      let s_len = Array.length s_arr in
      let search_len = Array.length search_arr in
      let start = max 0 start in
      if search_len > s_len || start > s_len - search_len then -1
      else
        let found = ref (-1) in
        let i = ref start in
        while !found = -1 && !i <= s_len - search_len do
          let matches = ref true in
          let j = ref 0 in
          while !matches && !j < search_len do
            if s_arr.(!i + !j) <> search_arr.(!j) then matches := false;
            incr j
          done;
          if !matches then found := !i;
          incr i
        done;
        !found

  (** lastIndexOf - find last occurrence. Optimized fast path for ASCII-only
      strings. *)
  let last_index_of search s =
    if Stdlib.String.length search = 0 then utf16_length s
    else if is_ascii s && is_ascii search then begin
      (* Fast path: for ASCII strings, byte indices = UTF-16 indices *)
      let s_len = Stdlib.String.length s in
      let search_len = Stdlib.String.length search in
      if search_len > s_len then -1
      else
        let rec find i =
          if i < 0 then -1
          else if Stdlib.String.sub s i search_len = search then i
          else find (i - 1)
        in
        find (s_len - search_len)
    end
    else begin
      let s_arr = to_utf16_array s in
      let search_arr = to_utf16_array search in
      let s_len = Array.length s_arr in
      let search_len = Array.length search_arr in
      if search_len > s_len then -1
      else
        let found = ref (-1) in
        for i = s_len - search_len downto 0 do
          let matches = ref true in
          let j = ref 0 in
          while !matches && !j < search_len do
            if s_arr.(i + !j) <> search_arr.(!j) then matches := false;
            incr j
          done;
          if !matches && !found = -1 then found := i
        done;
        !found
    end

  let last_index_of_from search pos s =
    if Stdlib.String.length search = 0 then clamp pos 0 (utf16_length s)
    else
      let s_arr = to_utf16_array s in
      let search_arr = to_utf16_array search in
      let s_len = Array.length s_arr in
      let search_len = Array.length search_arr in
      if search_len > s_len then -1
      else
        (* Per ECMA-262, negative positions are clamped to 0 *)
        let start = max 0 (min pos (s_len - search_len)) in
        let found = ref (-1) in
        for i = start downto 0 do
          let matches = ref true in
          let j = ref 0 in
          while !matches && !j < search_len do
            if s_arr.(i + !j) <> search_arr.(!j) then matches := false;
            incr j
          done;
          if !matches && !found = -1 then found := i
        done;
        !found

  (** includes - check if string contains search *)
  let includes search s = index_of search s >= 0

  let includes_from search pos s = index_of_from search pos s >= 0

  (** startsWith - check if string starts with search. Optimized fast path for
      ASCII-only strings. *)
  let starts_with search s =
    if is_ascii s && is_ascii search then begin
      (* Fast path: direct string comparison for ASCII *)
      let search_len = Stdlib.String.length search in
      let s_len = Stdlib.String.length s in
      search_len <= s_len && Stdlib.String.sub s 0 search_len = search
    end
    else begin
      let search_arr = to_utf16_array search in
      let s_arr = to_utf16_array s in
      let search_len = Array.length search_arr in
      let s_len = Array.length s_arr in
      if search_len > s_len then false
      else
        let matches = ref true in
        let i = ref 0 in
        while !matches && !i < search_len do
          if s_arr.(!i) <> search_arr.(!i) then matches := false;
          incr i
        done;
        !matches
    end

  let starts_with_from search pos s =
    let search_arr = to_utf16_array search in
    let s_arr = to_utf16_array s in
    let search_len = Array.length search_arr in
    let s_len = Array.length s_arr in
    let pos = clamp pos 0 s_len in
    (* compare via subtraction to avoid int overflow on huge [pos] *)
    if search_len > s_len - pos then false
    else
      let matches = ref true in
      let i = ref 0 in
      while !matches && !i < search_len do
        if s_arr.(pos + !i) <> search_arr.(!i) then matches := false;
        incr i
      done;
      !matches

  (** endsWith - check if string ends with search. Optimized fast path for
      ASCII-only strings. *)
  let ends_with search s =
    if is_ascii s && is_ascii search then begin
      (* Fast path: direct string comparison for ASCII *)
      let search_len = Stdlib.String.length search in
      let s_len = Stdlib.String.length s in
      search_len <= s_len
      && Stdlib.String.sub s (s_len - search_len) search_len = search
    end
    else begin
      let search_arr = to_utf16_array search in
      let s_arr = to_utf16_array s in
      let search_len = Array.length search_arr in
      let s_len = Array.length s_arr in
      if search_len > s_len then false
      else
        let start = s_len - search_len in
        let matches = ref true in
        let i = ref 0 in
        while !matches && !i < search_len do
          if s_arr.(start + !i) <> search_arr.(!i) then matches := false;
          incr i
        done;
        !matches
    end

  let ends_with_at search end_pos s =
    let search_arr = to_utf16_array search in
    let s_arr = to_utf16_array s in
    let search_len = Array.length search_arr in
    let s_len = Array.length s_arr in
    let end_pos = clamp end_pos 0 s_len in
    if search_len > end_pos then false
    else
      let start = end_pos - search_len in
      let matches = ref true in
      let i = ref 0 in
      while !matches && !i < search_len do
        if s_arr.(start + !i) <> search_arr.(!i) then matches := false;
        incr i
      done;
      !matches

  (** trim - remove leading and trailing whitespace *)
  let trim s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    if len = 0 then ""
    else begin
      let start = ref 0 in
      while !start < len && is_whitespace arr.(!start) do
        incr start
      done;
      let end_ = ref (len - 1) in
      while !end_ >= !start && is_whitespace arr.(!end_) do
        decr end_
      done;
      if !start > !end_ then ""
      else from_utf16_array (Array.sub arr !start (!end_ - !start + 1))
    end

  let trim_start s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    if len = 0 then ""
    else begin
      let start = ref 0 in
      while !start < len && is_whitespace arr.(!start) do
        incr start
      done;
      if !start >= len then ""
      else from_utf16_array (Array.sub arr !start (len - !start))
    end

  let trim_end s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    if len = 0 then ""
    else begin
      let end_ = ref (len - 1) in
      while !end_ >= 0 && is_whitespace arr.(!end_) do
        decr end_
      done;
      if !end_ < 0 then "" else from_utf16_array (Array.sub arr 0 (!end_ + 1))
    end

  (** padStart - pad to length with fill string *)
  let pad_start target_len s =
    let s_len = utf16_length s in
    if target_len <= s_len then s
    else
      let pad_len = target_len - s_len in
      let pad = Stdlib.String.make pad_len ' ' in
      pad ^ s

  let pad_start_with target_len fill_str s =
    if Stdlib.String.length fill_str = 0 then s
    else
      let s_len = utf16_length s in
      if target_len <= s_len then s
      else
        let pad_len = target_len - s_len in
        let fill_arr = to_utf16_array fill_str in
        let fill_len = Array.length fill_arr in
        let result = Array.make pad_len 0 in
        for i = 0 to pad_len - 1 do
          result.(i) <- fill_arr.(i mod fill_len)
        done;
        from_utf16_array result ^ s

  let pad_end target_len s =
    let s_len = utf16_length s in
    if target_len <= s_len then s
    else
      let pad_len = target_len - s_len in
      let pad = Stdlib.String.make pad_len ' ' in
      s ^ pad

  let pad_end_with target_len fill_str s =
    if Stdlib.String.length fill_str = 0 then s
    else
      let s_len = utf16_length s in
      if target_len <= s_len then s
      else
        let pad_len = target_len - s_len in
        let fill_arr = to_utf16_array fill_str in
        let fill_len = Array.length fill_arr in
        let result = Array.make pad_len 0 in
        for i = 0 to pad_len - 1 do
          result.(i) <- fill_arr.(i mod fill_len)
        done;
        s ^ from_utf16_array result

  (** repeat - repeat string n times *)
  let repeat n s =
    if n < 0 then raise (Invalid_argument "String.repeat: negative count")
    else if n = 0 || Stdlib.String.length s = 0 then ""
    else
      let buf = Stdlib.Buffer.create (Stdlib.String.length s * n) in
      for _ = 1 to n do
        Stdlib.Buffer.add_string buf s
      done;
      Stdlib.Buffer.contents buf

  (* ===== RegExp-based methods =====

     These methods compile their pattern argument on each call. An invalid
     pattern raises Invalid_argument, mirroring the SyntaxError JavaScript
     throws for invalid regular expression literals. *)

  let compile_exn ~fn ~flags pattern =
    match RegExp.compile ~flags pattern with
    | Ok re -> re
    | Error error ->
        invalid_arg
          (Printf.sprintf
             "String.Prototype.%s: invalid regular expression /%s/: %s" fn
             pattern
             (RegExp.compile_error_to_string error))

  let full_match (m : RegExp.match_result) =
    match m.RegExp.captures.(0) with Some s -> s | None -> ""

  (* Fold [f] over every match of a global regexp, advancing past empty
     matches so iteration always terminates. *)
  let fold_global_matches re s ~init ~f =
    let rec loop acc =
      match RegExp.exec re s with
      | None -> acc
      | Some m ->
          let acc = f acc m in
          if full_match m = "" then
            RegExp.set_last_index re (RegExp.last_index re + 1);
          loop acc
    in
    loop init

  (** match - find first match. Equivalent to JavaScript's
      String.prototype.match() without the global flag. *)
  let match_ pattern s =
    let re = compile_exn ~fn:"match_" ~flags:"" pattern in
    RegExp.exec re s

  let match_flags pattern flags s =
    if Stdlib.String.contains flags 'g' then
      invalid_arg
        "String.Prototype.match_flags: the 'g' flag returns multiple matches; \
         use match_global or match_all instead"
    else
      let re = compile_exn ~fn:"match_flags" ~flags pattern in
      RegExp.exec re s

  (** matchGlobal - all full matches, like JavaScript's match() with the global
      flag (which returns full matches only, no capture groups). Returns [||]
      when there is no match (JavaScript returns null). *)
  let match_global ?(flags = "") pattern s =
    let flags =
      if Stdlib.String.contains flags 'g' then flags else flags ^ "g"
    in
    let re = compile_exn ~fn:"match_global" ~flags pattern in
    fold_global_matches re s ~init:[] ~f:(fun acc m -> full_match m :: acc)
    |> List.rev |> Array.of_list

  (** matchAll - all matches with captures, groups and indices. Equivalent to
      JavaScript's String.prototype.matchAll(). *)
  let match_all ?(flags = "") pattern s =
    let flags =
      if Stdlib.String.contains flags 'g' then flags else flags ^ "g"
    in
    let re = compile_exn ~fn:"match_all" ~flags pattern in
    fold_global_matches re s ~init:[] ~f:(fun acc m -> m :: acc) |> List.rev

  (** search - find UTF-16 index of first match *)
  let search pattern s =
    let re = compile_exn ~fn:"search" ~flags:"" pattern in
    match RegExp.exec re s with Some m -> m.RegExp.index | None -> -1

  let search_flags pattern flags s =
    let re = compile_exn ~fn:"search_flags" ~flags pattern in
    match RegExp.exec re s with Some m -> m.RegExp.index | None -> -1

  (** Process JavaScript replacement patterns in a replacement string.
      - $$ -> literal $
      - $& -> matched substring
      - $` -> portion before match
      - $' -> portion after match
      - $n or $nn -> capture group n (for regex) *)
  let process_replacement ~replacement ~matched ~before_match ~after_match
      ~captures =
    let len = Stdlib.String.length replacement in
    if len = 0 then ""
    else
      let buf = Stdlib.Buffer.create len in
      let i = ref 0 in
      while !i < len do
        let c = Stdlib.String.get replacement !i in
        if c = '$' && !i + 1 < len then begin
          let next = Stdlib.String.get replacement (!i + 1) in
          match next with
          | '$' ->
              Stdlib.Buffer.add_char buf '$';
              i := !i + 2
          | '&' ->
              Stdlib.Buffer.add_string buf matched;
              i := !i + 2
          | '`' ->
              Stdlib.Buffer.add_string buf before_match;
              i := !i + 2
          | '\'' ->
              Stdlib.Buffer.add_string buf after_match;
              i := !i + 2
          | '0' .. '9' ->
              (* Parse capture group number - could be 1 or 2 digits *)
              let digit1 = Char.code next - Char.code '0' in
              if !i + 2 < len then
                let next2 = Stdlib.String.get replacement (!i + 2) in
                match next2 with
                | '0' .. '9' ->
                    let digit2 = Char.code next2 - Char.code '0' in
                    let n = (digit1 * 10) + digit2 in
                    if n < Array.length captures && n > 0 then begin
                      Stdlib.Buffer.add_string buf captures.(n);
                      i := !i + 3
                    end
                    else if digit1 < Array.length captures && digit1 > 0 then begin
                      Stdlib.Buffer.add_string buf captures.(digit1);
                      i := !i + 2
                    end
                    else begin
                      Stdlib.Buffer.add_char buf '$';
                      incr i
                    end
                | _ ->
                    if digit1 < Array.length captures && digit1 > 0 then begin
                      Stdlib.Buffer.add_string buf captures.(digit1);
                      i := !i + 2
                    end
                    else begin
                      Stdlib.Buffer.add_char buf '$';
                      incr i
                    end
              else if digit1 < Array.length captures && digit1 > 0 then begin
                Stdlib.Buffer.add_string buf captures.(digit1);
                i := !i + 2
              end
              else begin
                Stdlib.Buffer.add_char buf '$';
                incr i
              end
          | _ ->
              Stdlib.Buffer.add_char buf '$';
              incr i
        end
        else begin
          Stdlib.Buffer.add_char buf c;
          incr i
        end
      done;
      Stdlib.Buffer.contents buf

  (* Captures for process_replacement: JavaScript substitutes the empty
     string for capture groups that did not participate. *)
  let replacement_captures (m : RegExp.match_result) =
    Array.map
      (fun capture -> match capture with Some s -> s | None -> "")
      m.RegExp.captures

  (** replace - replace first occurrence *)
  let replace search replacement s =
    let idx = index_of search s in
    if idx < 0 then s
    else
      let search_len = utf16_length search in
      let before_match = slice ~start:0 ~end_:idx s in
      let after_match = slice_from (idx + search_len) s in
      let processed =
        process_replacement ~replacement ~matched:search ~before_match
          ~after_match ~captures:[| search |]
      in
      before_match ^ processed ^ after_match

  let replace_regex_with_flags ~fn pattern flags replacement s =
    let re = compile_exn ~fn ~flags pattern in
    match RegExp.exec re s with
    | None -> s
    | Some m ->
        let match_str = full_match m in
        let idx = m.RegExp.index in
        let match_len = utf16_length match_str in
        let before_match = slice ~start:0 ~end_:idx s in
        let after_match = slice_from (idx + match_len) s in
        let processed =
          process_replacement ~replacement ~matched:match_str ~before_match
            ~after_match ~captures:(replacement_captures m)
        in
        before_match ^ processed ^ after_match

  let replace_regex pattern replacement s =
    replace_regex_with_flags ~fn:"replace_regex" pattern "" replacement s

  let replace_regex_flags pattern flags replacement s =
    replace_regex_with_flags ~fn:"replace_regex_flags" pattern flags replacement
      s

  let replace_regex_global pattern replacement s =
    let re = compile_exn ~fn:"replace_regex_global" ~flags:"g" pattern in
    let parts, last_end =
      fold_global_matches re s ~init:([], 0) ~f:(fun (parts, last_end) m ->
          let match_str = full_match m in
          let idx = m.RegExp.index in
          let match_end = idx + utf16_length match_str in
          let before = slice ~start:last_end ~end_:idx s in
          let processed =
            process_replacement ~replacement ~matched:match_str
              ~before_match:(slice ~start:0 ~end_:idx s)
              ~after_match:(slice_from match_end s)
              ~captures:(replacement_captures m)
          in
          (processed :: before :: parts, match_end))
    in
    let parts = slice_from last_end s :: parts in
    Stdlib.String.concat "" (List.rev parts)

  (** replaceAll - replace all occurrences *)
  let replace_all search replacement s =
    if Stdlib.String.length search = 0 then begin
      (* Insert replacement between every character and at start/end *)
      let arr = to_utf16_array s in
      let len = Array.length arr in
      if len = 0 then
        process_replacement ~replacement ~matched:"" ~before_match:""
          ~after_match:"" ~captures:[| "" |]
      else begin
        let parts = ref [] in
        (* Process each position *)
        for i = len - 1 downto 0 do
          let char_str = from_utf16_array [| arr.(i) |] in
          let before_match = from_utf16_array (Array.sub arr 0 i) in
          let after_match =
            from_utf16_array (Array.sub arr (i + 1) (len - i - 1))
          in
          let processed =
            process_replacement ~replacement ~matched:"" ~before_match
              ~after_match ~captures:[| "" |]
          in
          parts := char_str :: processed :: !parts
        done;
        (* Add replacement at start *)
        let first_processed =
          process_replacement ~replacement ~matched:"" ~before_match:""
            ~after_match:s ~captures:[| "" |]
        in
        Stdlib.String.concat "" (first_processed :: !parts)
      end
    end
    else
      let search_len = utf16_length search in
      (* [pos] is a UTF-16 position in the original string; $` and $' always
         refer to the original string, per the JavaScript spec *)
      let rec loop pos acc =
        let idx = index_of_from search pos s in
        if idx < 0 then List.rev (slice_from pos s :: acc)
        else
          let before = slice ~start:pos ~end_:idx s in
          let processed =
            process_replacement ~replacement ~matched:search
              ~before_match:(slice ~start:0 ~end_:idx s)
              ~after_match:(slice_from (idx + search_len) s)
              ~captures:[| search |]
          in
          loop (idx + search_len) (processed :: before :: acc)
      in
      Stdlib.String.concat "" (loop 0 [])

  let replace_all_regex pattern replacement s =
    replace_regex_global pattern replacement s

  (** split - split string by separator *)
  let split sep s =
    if Stdlib.String.length sep = 0 then begin
      (* Split into individual UTF-16 code units *)
      let arr = to_utf16_array s in
      Array.map (fun c -> from_utf16_array [| c |]) arr
    end
    else begin
      let sep_len = utf16_length sep in
      let rec loop s acc =
        let idx = index_of sep s in
        if idx < 0 then Array.of_list (List.rev (s :: acc))
        else
          let before = slice ~start:0 ~end_:idx s in
          let after = slice_from (idx + sep_len) s in
          loop after (before :: acc)
      in
      loop s []
    end

  let split_limit sep limit s =
    if limit <= 0 then [||]
    else if Stdlib.String.length sep = 0 then begin
      let arr = to_utf16_array s in
      let len = min limit (Array.length arr) in
      Array.init len (fun i -> from_utf16_array [| arr.(i) |])
    end
    else begin
      let sep_len = utf16_length sep in
      let rec loop s acc count =
        if count >= limit then Array.of_list (List.rev acc)
        else
          let idx = index_of sep s in
          if idx < 0 then Array.of_list (List.rev (s :: acc))
          else
            let before = slice ~start:0 ~end_:idx s in
            let after = slice_from (idx + sep_len) s in
            loop after (before :: acc) (count + 1)
      in
      loop s [] 0
    end

  (** split by regex pattern. Capture groups are spliced into the result (as in
      JavaScript); groups that did not participate contribute [""]. *)
  let split_regex_nonempty re s =
    let s_len = utf16_length s in
    let parts = ref [] in
    let last_end = ref 0 in
    let continue = ref true in
    while !continue do
      match RegExp.exec re s with
      | None -> continue := false
      | Some m ->
          let match_str = full_match m in
          let idx = m.RegExp.index in
          let match_end = idx + utf16_length match_str in
          if idx >= s_len then
            (* Per the spec's SplitMatch loop, positions beyond the last
               character are never attempted *)
            continue := false
          else begin
            if match_end = !last_end then
              (* Empty match adjacent to the previous split point: skip it
                 (ECMA-262 SplitMatch: only advancing matches split) *)
              ()
            else begin
              parts := slice ~start:!last_end ~end_:idx s :: !parts;
              for i = 1 to Array.length m.RegExp.captures - 1 do
                let capture =
                  match m.RegExp.captures.(i) with Some c -> c | None -> ""
                in
                parts := capture :: !parts
              done;
              last_end := match_end
            end;
            if match_str = "" then
              RegExp.set_last_index re (RegExp.last_index re + 1)
          end
    done;
    parts := slice_from !last_end s :: !parts;
    Array.of_list (List.rev !parts)

  let split_regex pattern s =
    let re = compile_exn ~fn:"split_regex" ~flags:"g" pattern in
    if s = "" then
      (* Per spec: splitting the empty string yields [] when the separator
         matches it, and [""] otherwise *)
      match RegExp.exec re s with
      | Some _ -> [||]
      | None -> [| s |]
    else split_regex_nonempty re s
end

(* Character-level case conversion *)
let lowercase_char = Unicode.lowercase_char
let uppercase_char = Unicode.uppercase_char
