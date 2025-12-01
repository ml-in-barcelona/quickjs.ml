(* Benchmark for quickjs.ml bindings *)

module RegExp = Quickjs.RegExp
module Unicode = Quickjs.Unicode
module Number = Quickjs.Number
module Global = Quickjs.Global

type result = { time_per_op_us : float; words_per_op : float }

let bench ?(iterations = 100_000) f =
  for _ = 1 to 1000 do
    ignore (f ())
  done;
  (* warmup *)
  Gc.full_major ();
  Gc.compact ();
  let stat_before = Gc.quick_stat () in
  let start = Unix.gettimeofday () in
  for _ = 1 to iterations do
    ignore (f ())
  done;
  let elapsed = Unix.gettimeofday () -. start in
  let stat_after = Gc.quick_stat () in
  let words = stat_after.minor_words -. stat_before.minor_words in
  {
    time_per_op_us = elapsed /. float_of_int iterations *. 1_000_000.0;
    words_per_op = words /. float_of_int iterations;
  }

let print name r =
  Printf.printf "  %-44s %8.3f µs  %8.1f words\n%!" name r.time_per_op_us
    r.words_per_op

let compare ~name ~ours ~theirs =
  let ratio = theirs.time_per_op_us /. ours.time_per_op_us in
  let s =
    if ratio >= 1.0 then Printf.sprintf "%.1fx faster" ratio
    else Printf.sprintf "%.1fx slower" (1.0 /. ratio)
  in
  Printf.printf "  %-44s %s\n%!" name s

let section s = Printf.printf "\n[%s]\n" s

let header s n =
  Printf.printf "\n%s (%d iterations)\n%s\n" s n (String.make 70 '=')

let regexp p ~flags =
  match RegExp.compile p ~flags with
  | Ok re -> re
  | Error e -> failwith (RegExp.compile_error_to_string e)

(* === QUICK BENCHMARK === *)
let run_quick () =
  header "quickjs.ml quick benchmark" 100_000;

  section "RegExp";
  print "compile simple" (bench (fun () -> regexp "[0-9]+" ~flags:""));
  print "compile complex"
    (bench (fun () ->
         regexp "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})" ~flags:"g"));
  let re = regexp "[0-9]+" ~flags:"" in
  print "test (match)" (bench (fun () -> RegExp.test re "abc123xyz"));
  print "test (no match)" (bench (fun () -> RegExp.test re "abcxyz"));
  let date_re =
    regexp "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})" ~flags:""
  in
  print "exec with captures"
    (bench (fun () -> RegExp.exec date_re "2024-07-17"));

  section "Unicode";
  print "lowercase ASCII" (bench (fun () -> Unicode.lowercase "HELLO WORLD"));
  print "lowercase Unicode" (bench (fun () -> Unicode.lowercase "ÉCOLE CAFÉ"));
  print "uppercase ASCII" (bench (fun () -> Unicode.uppercase "hello world"));
  print "normalize NFC" (bench (fun () -> Unicode.normalize Unicode.NFC "café"));
  print "is_id_start"
    (bench (fun () -> Unicode.is_id_start (Uchar.of_char 'a')));

  section "Number.Prototype (float to string)";
  print "to_string integer" (bench (fun () -> Number.Prototype.to_string 42.0));
  print "to_string decimal"
    (bench (fun () -> Number.Prototype.to_string 3.14159265358979));
  print "to_fixed 2" (bench (fun () -> Number.Prototype.to_fixed 2 3.14159));

  section "Global (parse float)";
  print "parse integer" (bench (fun () -> Global.parse_float "12345"));
  print "parse decimal" (bench (fun () -> Global.parse_float "3.14159"));

  section "Number (int to string)";
  print "of_int" (bench (fun () -> Number.of_int 123456789));
  print "of_int64" (bench (fun () -> Number.of_int64 9223372036854775807L));
  print "of_int_radix 16" (bench (fun () -> Number.of_int_radix ~radix:16 255))

(* === STDLIB COMPARISON === *)
let run_compare () =
  header "quickjs.ml vs stdlib comparison" 100_000;

  section "RegExp vs Str";
  let our_re = regexp "[0-9]+" ~flags:"" in
  let str_re = Str.regexp "[0-9]+" in
  let ours = bench (fun () -> RegExp.test our_re "abc123xyz") in
  let theirs = bench (fun () -> Str.string_match str_re "abc123xyz" 0) in
  print "quickjs.RegExp.test" ours;
  print "Str.string_match" theirs;
  compare ~name:"-> RegExp" ~ours ~theirs;
  Printf.printf "  (quickjs has: lookbehind, named groups, \\p{L}, unicode)\n";

  section "Unicode vs String";
  let ours = bench (fun () -> Unicode.lowercase "HELLO WORLD") in
  let theirs = bench (fun () -> String.lowercase_ascii "HELLO WORLD") in
  print "quickjs.Unicode.lowercase" ours;
  print "String.lowercase_ascii" theirs;
  compare ~name:"-> Unicode" ~ours ~theirs;
  Printf.printf "  (quickjs handles full Unicode, stdlib is ASCII-only)\n";

  section "Number.Prototype vs stdlib";
  let ours = bench (fun () -> Number.Prototype.to_string 3.14159) in
  let theirs = bench (fun () -> string_of_float 3.14159) in
  print "quickjs.Number.Prototype.to_string" ours;
  print "string_of_float" theirs;
  compare ~name:"-> float->string" ~ours ~theirs;

  let ours = bench (fun () -> Global.parse_float "3.14159") in
  let theirs = bench (fun () -> float_of_string "3.14159") in
  print "quickjs.Global.parse_float" ours;
  print "float_of_string" theirs;
  compare ~name:"-> string->float" ~ours ~theirs;

  section "Number vs stdlib";
  let ours = bench (fun () -> Number.of_int 123456789) in
  let theirs = bench (fun () -> Int.to_string 123456789) in
  print "quickjs.Number.of_int" ours;
  print "Int.to_string" theirs;
  compare ~name:"-> int->string" ~ours ~theirs;

  let ours = bench (fun () -> Number.of_int64 9223372036854775807L) in
  let theirs = bench (fun () -> Int64.to_string 9223372036854775807L) in
  print "quickjs.Number.of_int64" ours;
  print "Int64.to_string" theirs;
  compare ~name:"-> int64->string" ~ours ~theirs

(* === SCALING BENCHMARK === *)
let run_scaling () =
  header "quickjs.ml scaling benchmark" 10_000;

  let make_digits n = String.init n (fun i -> Char.chr (48 + (i mod 10))) in
  let make_upper n = String.make n 'A' in

  section "RegExp.test - input size scaling";
  let re = regexp "[0-9]+" ~flags:"" in
  [ 10; 100; 1000; 5000 ]
  |> List.iter (fun size ->
      let input = "abc" ^ make_digits size ^ "xyz" in
      print
        (Printf.sprintf "%d digits" size)
        (bench ~iterations:10_000 (fun () -> RegExp.test re input)));

  section "Unicode.lowercase - input size scaling";
  [ 10; 100; 1000; 5000 ]
  |> List.iter (fun size ->
      let input = make_upper size in
      print
        (Printf.sprintf "%d chars" size)
        (bench ~iterations:10_000 (fun () -> Unicode.lowercase input)));

  section "Number.Prototype - number magnitude";
  [
    ("42", 42.0);
    ("123456789", 123456789.0);
    ("1e20", 1e20);
    ("1e100", 1e100);
    ("1e-100", 1e-100);
    ("pi", 3.141592653589793);
  ]
  |> List.iter (fun (name, num) ->
      print name (bench (fun () -> Number.Prototype.to_string num)));

  section "Number - radix";
  [ 2; 8; 10; 16; 36 ]
  |> List.iter (fun radix ->
      print
        (Printf.sprintf "radix %d" radix)
        (bench (fun () -> Number.of_int_radix ~radix 123456789)))

(* === EDGE CASES === *)
let run_edge () =
  header "quickjs.ml edge cases" 100_000;

  section "Empty/minimal inputs";
  print "lowercase empty" (bench (fun () -> Unicode.lowercase ""));
  print "lowercase 1 char" (bench (fun () -> Unicode.lowercase "A"));
  print "parse empty" (bench (fun () -> Global.parse_float ""));
  print "of_int 0" (bench (fun () -> Number.of_int 0));

  section "Special floats";
  print "to_string NaN" (bench (fun () -> Number.Prototype.to_string Float.nan));
  print "to_string Infinity"
    (bench (fun () -> Number.Prototype.to_string Float.infinity));
  print "to_string -Infinity"
    (bench (fun () -> Number.Prototype.to_string Float.neg_infinity));
  print "to_string -0.0" (bench (fun () -> Number.Prototype.to_string (-0.0)));

  section "Unicode edge cases";
  print "German eszett uppercase" (bench (fun () -> Unicode.uppercase "ß"));
  print "normalize decomposed e+accent"
    (bench (fun () -> Unicode.normalize Unicode.NFC "e\xCC\x81"));
  print "is_id_start emoji"
    (bench (fun () -> Unicode.is_id_start (Uchar.of_int 0x1F600)));

  section "Boundary integers";
  print "max_int" (bench (fun () -> Number.of_int max_int));
  print "min_int" (bench (fun () -> Number.of_int min_int));
  print "int64 max" (bench (fun () -> Number.of_int64 Int64.max_int));
  print "int64 min" (bench (fun () -> Number.of_int64 Int64.min_int))

(* === MAIN === *)
let usage =
  {|
Usage: benchmark [SCENARIO]

Scenarios:
  quick    Fast overview of all operations (default)
  compare  Compare against OCaml stdlib
  scaling  Test performance with varying input sizes
  edge     Edge cases and special values
  all      Run all scenarios
|}

let () =
  let scenario = if Array.length Sys.argv > 1 then Sys.argv.(1) else "quick" in
  match scenario with
  | "quick" -> run_quick ()
  | "compare" -> run_compare ()
  | "scaling" -> run_scaling ()
  | "edge" -> run_edge ()
  | "all" ->
      run_quick ();
      run_compare ();
      run_scaling ();
      run_edge ()
  | _ ->
      Printf.printf "%s" usage;
      exit 1
