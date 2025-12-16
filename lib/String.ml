(** JavaScript String built-in object

    This module mirrors the JavaScript String API with prototype methods for
    string manipulation. All methods use UTF-16 semantics for indices. *)

(* Re-export normalization type from Unicode *)
type normalization = Unicode.normalization = NFC | NFD | NFKC | NFKD

(* UTF-16 helper functions *)

(** Get the UTF-16 length of a string (number of code units) *)
let utf16_length s =
  let len = Stdlib.String.length s in
  let count = ref 0 in
  let rec loop i =
    if i >= len then !count
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let code = Uchar.to_int u in
      if code >= 0x10000 then count := !count + 2 else incr count;
      loop (i + Uchar.utf_decode_length d)
  in
  loop 0

(** Convert UTF-8 string to array of UTF-16 code units *)
let to_utf16_array s =
  let len = Stdlib.String.length s in
  let units = ref [] in
  let rec loop i =
    if i >= len then Array.of_list (List.rev !units)
    else
      let d = Stdlib.String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let code = Uchar.to_int u in
      if code >= 0x10000 then begin
        (* Surrogate pair - add high first, then low, so after List.rev we get [high; low] *)
        let code' = code - 0x10000 in
        units := (0xD800 lor (code' lsr 10)) :: !units;
        units := (0xDC00 lor (code' land 0x3FF)) :: !units
      end
      else units := code :: !units;
      loop (i + Uchar.utf_decode_length d)
  in
  loop 0

(** Convert array of UTF-16 code units back to UTF-8 string *)
let from_utf16_array arr =
  let buf = Stdlib.Buffer.create (Array.length arr * 3) in
  let len = Array.length arr in
  let i = ref 0 in
  while !i < len do
    let code = arr.(!i) in
    if code >= 0xD800 && code <= 0xDBFF && !i + 1 < len then begin
      (* High surrogate, check for low *)
      let low = arr.(!i + 1) in
      if low >= 0xDC00 && low <= 0xDFFF then begin
        let cp = 0x10000 + ((code - 0xD800) * 0x400) + (low - 0xDC00) in
        Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
        i := !i + 2
      end
      else begin
        Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_int code);
        incr i
      end
    end
    else begin
      let u =
        if
          code >= 0 && code <= 0x10FFFF && not (code >= 0xD800 && code <= 0xDFFF)
        then Uchar.of_int code
        else Uchar.rep
      in
      Stdlib.Buffer.add_utf_8_uchar buf u;
      incr i
    end
  done;
  Stdlib.Buffer.contents buf

module Prototype = struct
  (** String.prototype methods *)

  let to_lower_case = Unicode.lowercase
  let to_upper_case = Unicode.uppercase
  let normalize = Unicode.normalize

  (** Get the UTF-16 length of a string *)
  let length = utf16_length

  (** Get character at UTF-16 index *)
  let char_at idx s =
    if idx < 0 then ""
    else
      let arr = to_utf16_array s in
      if idx >= Array.length arr then "" else from_utf16_array [| arr.(idx) |]

  (** Get UTF-16 code unit at index *)
  let char_code_at idx s =
    if idx < 0 then None
    else
      let arr = to_utf16_array s in
      if idx >= Array.length arr then None else Some arr.(idx)

  (** Get full Unicode code point at UTF-16 index *)
  let code_point_at idx s =
    if idx < 0 then None
    else
      let arr = to_utf16_array s in
      let len = Array.length arr in
      if idx >= len then None
      else
        let code = arr.(idx) in
        if code >= 0xD800 && code <= 0xDBFF && idx + 1 < len then
          let low = arr.(idx + 1) in
          if low >= 0xDC00 && low <= 0xDFFF then
            Some (0x10000 + ((code - 0xD800) * 0x400) + (low - 0xDC00))
          else Some code
        else Some code

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

  (** indexOf - find first occurrence *)
  let index_of search s =
    if Stdlib.String.length search = 0 then 0
    else
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

  (** lastIndexOf - find last occurrence *)
  let last_index_of search s =
    if Stdlib.String.length search = 0 then utf16_length s
    else
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

  (** startsWith - check if string starts with search *)
  let starts_with search s =
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

  (** endsWith - check if string ends with search *)
  let ends_with search s =
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
    else
      let is_ws code =
        code = 0x20 || code = 0x09 || code = 0x0A || code = 0x0B || code = 0x0C
        || code = 0x0D || code = 0xA0 || code = 0xFEFF
        || (code >= 0x2000 && code <= 0x200A)
        || code = 0x2028 || code = 0x2029 || code = 0x202F || code = 0x205F
        || code = 0x3000
      in
      let start = ref 0 in
      while !start < len && is_ws arr.(!start) do
        incr start
      done;
      let end_ = ref (len - 1) in
      while !end_ >= !start && is_ws arr.(!end_) do
        decr end_
      done;
      if !start > !end_ then ""
      else from_utf16_array (Array.sub arr !start (!end_ - !start + 1))

  let trim_start s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    if len = 0 then ""
    else
      let is_ws code =
        code = 0x20 || code = 0x09 || code = 0x0A || code = 0x0B || code = 0x0C
        || code = 0x0D || code = 0xA0 || code = 0xFEFF
        || (code >= 0x2000 && code <= 0x200A)
        || code = 0x2028 || code = 0x2029 || code = 0x202F || code = 0x205F
        || code = 0x3000
      in
      let start = ref 0 in
      while !start < len && is_ws arr.(!start) do
        incr start
      done;
      if !start >= len then ""
      else from_utf16_array (Array.sub arr !start (len - !start))

  let trim_end s =
    let arr = to_utf16_array s in
    let len = Array.length arr in
    if len = 0 then ""
    else
      let is_ws code =
        code = 0x20 || code = 0x09 || code = 0x0A || code = 0x0B || code = 0x0C
        || code = 0x0D || code = 0xA0 || code = 0xFEFF
        || (code >= 0x2000 && code <= 0x200A)
        || code = 0x2028 || code = 0x2029 || code = 0x202F || code = 0x205F
        || code = 0x3000
      in
      let end_ = ref (len - 1) in
      while !end_ >= 0 && is_ws arr.(!end_) do
        decr end_
      done;
      if !end_ < 0 then "" else from_utf16_array (Array.sub arr 0 (!end_ + 1))

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
              RegExp.setLastIndex re (RegExp.lastIndex re + 1)
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
              RegExp.setLastIndex re (RegExp.lastIndex re + 1)
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
        RegExp.setLastIndex re 0;
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
              RegExp.setLastIndex re !last_index
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
              RegExp.setLastIndex re !last_index
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
