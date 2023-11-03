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
