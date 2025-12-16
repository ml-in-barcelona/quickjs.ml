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

(** JavaScript whitespace characters (per ECMA-262). Includes ASCII whitespace,
    Unicode spaces, and line terminators. *)
let is_whitespace code =
  code = 0x20 (* space *) || code = 0x09 (* tab *)
  || code = 0x0A (* line feed *)
  || code = 0x0B (* vertical tab *)
  || code = 0x0C (* form feed *)
  || code = 0x0D (* carriage return *)
  || code = 0xA0 (* non-breaking space *)
  || code = 0xFEFF (* BOM / zero-width no-break space *)
  || (code >= 0x2000 && code <= 0x200A) (* various Unicode spaces *)
  || code = 0x2028 (* line separator *)
  || code = 0x2029 (* paragraph separator *)
  || code = 0x202F (* narrow no-break space *)
  || code = 0x205F (* medium mathematical space *)
  || code = 0x3000 (* ideographic space *)

(** Convert array of UTF-16 code units back to UTF-8 string. Tail-recursive for
    stack safety. Handles surrogate pairs correctly. *)
let from_utf16_array arr =
  let len = Array.length arr in
  if len = 0 then ""
  else begin
    let buf = Stdlib.Buffer.create (len * 3) in
    let rec loop i =
      if i >= len then Stdlib.Buffer.contents buf
      else
        let code = arr.(i) in
        if code >= 0xD800 && code <= 0xDBFF && i + 1 < len then begin
          (* High surrogate, check for low *)
          let low = arr.(i + 1) in
          if low >= 0xDC00 && low <= 0xDFFF then begin
            let cp = 0x10000 + ((code - 0xD800) * 0x400) + (low - 0xDC00) in
            Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
            loop (i + 2)
          end
          else begin
            (* Lone high surrogate - output as-is (invalid but we handle it) *)
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
    let pos = max 0 pos in
    if pos + search_len > s_len then false
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

  (** match - find first match *)
  let match_ pattern s =
    match RegExp.compile ~flags:"" pattern with
    | Error _ -> None
    | Ok re ->
        let result = RegExp.exec re s in
        let caps = RegExp.captures result in
        if Array.length caps = 0 then None else Some caps

  let match_flags pattern flags s =
    match RegExp.compile ~flags pattern with
    | Error _ -> None
    | Ok re ->
        let result = RegExp.exec re s in
        let caps = RegExp.captures result in
        if Array.length caps = 0 then None else Some caps

  let match_global pattern s =
    match RegExp.compile ~flags:"g" pattern with
    | Error _ -> [||]
    | Ok re ->
        let matches = ref [] in
        let continue = ref true in
        while !continue do
          let result = RegExp.exec re s in
          let caps = RegExp.captures result in
          if Array.length caps = 0 then continue := false
          else begin
            matches := caps.(0) :: !matches;
            if caps.(0) = "" then
              RegExp.set_last_index re (RegExp.lastIndex re + 1)
          end
        done;
        Array.of_list (List.rev !matches)

  type match_result = {
    full_match : string;
    captures : string array;
    groups : (string * string) list;
    index : int;
  }
  (** matchAll result type *)

  let match_all pattern s =
    match RegExp.compile ~flags:"g" pattern with
    | Error _ -> []
    | Ok re ->
        let matches = ref [] in
        let continue = ref true in
        while !continue do
          let result = RegExp.exec re s in
          let caps = RegExp.captures result in
          if Array.length caps = 0 then continue := false
          else begin
            matches :=
              {
                full_match = caps.(0);
                captures = caps;
                groups = RegExp.groups result;
                index = RegExp.index result;
              }
              :: !matches;
            if caps.(0) = "" then
              RegExp.set_last_index re (RegExp.lastIndex re + 1)
          end
        done;
        List.rev !matches

  (** search - find index of first match *)
  let search pattern s =
    match RegExp.compile ~flags:"" pattern with
    | Error _ -> -1
    | Ok re ->
        let result = RegExp.exec re s in
        let caps = RegExp.captures result in
        if Array.length caps = 0 then -1 else RegExp.index result

  let search_flags pattern flags s =
    match RegExp.compile ~flags pattern with
    | Error _ -> -1
    | Ok re ->
        let result = RegExp.exec re s in
        let caps = RegExp.captures result in
        if Array.length caps = 0 then -1 else RegExp.index result

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

  let replace_regex pattern replacement s =
    match RegExp.compile ~flags:"" pattern with
    | Error _ -> s
    | Ok re ->
        let result = RegExp.exec re s in
        let caps = RegExp.captures result in
        if Array.length caps = 0 then s
        else
          let match_str = caps.(0) in
          let idx = RegExp.index result in
          let match_len = utf16_length match_str in
          let before_match = slice ~start:0 ~end_:idx s in
          let after_match = slice_from (idx + match_len) s in
          let processed =
            process_replacement ~replacement ~matched:match_str ~before_match
              ~after_match ~captures:caps
          in
          before_match ^ processed ^ after_match

  let replace_regex_global pattern replacement s =
    match RegExp.compile ~flags:"g" pattern with
    | Error _ -> s
    | Ok re ->
        let current = ref s in
        let last_index = ref 0 in
        let continue = ref true in
        let parts = ref [] in
        RegExp.set_last_index re 0;
        while !continue do
          let match_result = RegExp.exec re !current in
          let caps = RegExp.captures match_result in
          if Array.length caps = 0 then continue := false
          else begin
            let match_str = caps.(0) in
            let idx = RegExp.index match_result in
            let before_match = slice ~start:!last_index ~end_:idx !current in
            let match_end = idx + utf16_length match_str in
            let after_match = slice_from match_end !current in
            let processed =
              process_replacement ~replacement ~matched:match_str
                ~before_match:(slice ~start:0 ~end_:idx !current)
                ~after_match ~captures:caps
            in
            parts := processed :: before_match :: !parts;
            last_index := match_end;
            if match_str = "" then begin
              last_index := !last_index + 1;
              RegExp.set_last_index re !last_index
            end
          end
        done;
        let remaining = slice_from !last_index !current in
        parts := remaining :: !parts;
        Stdlib.String.concat "" (List.rev !parts)

  let replace_regex_flags pattern flags replacement s =
    match RegExp.compile ~flags pattern with
    | Error _ -> s
    | Ok re ->
        let result = RegExp.exec re s in
        let caps = RegExp.captures result in
        if Array.length caps = 0 then s
        else
          let match_str = caps.(0) in
          let idx = RegExp.index result in
          let match_len = utf16_length match_str in
          let before_match = slice ~start:0 ~end_:idx s in
          let after_match = slice_from (idx + match_len) s in
          let processed =
            process_replacement ~replacement ~matched:match_str ~before_match
              ~after_match ~captures:caps
          in
          before_match ^ processed ^ after_match

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
      let rec loop current_s acc_parts original_s =
        let idx = index_of search current_s in
        if idx < 0 then
          Stdlib.String.concat "" (List.rev (current_s :: acc_parts))
        else
          let before_match = slice ~start:0 ~end_:idx current_s in
          let after_match = slice_from (idx + search_len) current_s in
          let processed =
            process_replacement ~replacement ~matched:search
              ~before_match:
                ( acc_parts |> List.rev |> Stdlib.String.concat ""
                |> fun prefix -> prefix ^ before_match )
              ~after_match ~captures:[| search |]
          in
          loop after_match (processed :: before_match :: acc_parts) original_s
      in
      loop s [] s

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

  let split_regex pattern s =
    match RegExp.compile ~flags:"g" pattern with
    | Error _ -> [| s |]
    | Ok re ->
        let parts = ref [] in
        let last_index = ref 0 in
        let continue = ref true in
        while !continue do
          let result = RegExp.exec re s in
          let caps = RegExp.captures result in
          if Array.length caps = 0 then continue := false
          else begin
            let match_str = caps.(0) in
            let idx = RegExp.index result in
            let before = slice ~start:!last_index ~end_:idx s in
            parts := before :: !parts;
            (* Add captured groups *)
            for i = 1 to Array.length caps - 1 do
              parts := caps.(i) :: !parts
            done;
            last_index := idx + utf16_length match_str;
            if match_str = "" then begin
              last_index := !last_index + 1;
              RegExp.set_last_index re !last_index
            end
          end
        done;
        let remaining = slice_from !last_index s in
        parts := remaining :: !parts;
        Array.of_list (List.rev !parts)
end

(* Character-level case conversion *)
let lowercase_char = Unicode.lowercase_char
let uppercase_char = Unicode.uppercase_char
