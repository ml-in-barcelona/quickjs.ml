# quickjs

**quickjs.ml** is a set of OCaml bindings to some libraries from [QuickJS](https://quickjs-ng.github.io/quickjs/), a small and embeddable JavaScript engine that aims to support the latest ECMAScript specification.

This project exposes two libraries:

- **[`quickjs.c`](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/c/index.html)**: Low-level OCaml bindings to QuickJS C libs (`libregexp`, `libunicode`, `js_dtoa`, `js_atod`, and `cutils`)

- **[`quickjs`](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/Quickjs/index.html)**: A high-level API that mirrors JavaScript's built-in objects and methods. Modules include `RegExp`, `String`, `Number`, `Global`, and `Unicode`.

### Motivation

OCaml can target both native code and JavaScript (via [Melange](https://melange.re)), enabling "universal" code that runs on both platforms. However, JavaScript's specific behavior (such as Unicode, number formatting, and regular expressions) often differs between native OCaml and browser engines. Rather than reimplementing these complex behaviors from scratch, we leverage QuickJS.

This project bridges that gap by bringing the same behavior as JavaScript engines ([SpiderMonkey](https://spidermonkey.dev), [JavaScriptCore](https://developer.apple.com/documentation/javascriptcore), [ChakraCore](https://github.com/chakra-core/ChakraCore), [v8](https://v8.dev)) into native OCaml. With `quickjs.ml`, universal code produces identical results whether it runs natively or in the browser.

#### TC39/test262 Compatibility

We are translating [TC39/test262](https://github.com/tc39/test262) tests into OCaml to ensure full compatibility with the ECMAScript specification.

### Documentation

[API reference](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/index.html)

### Usage

Add `quickjs` library in dune
```clojure
(libraries quickjs)
```

All string indices (match positions, `last_index`, `String.Prototype` methods) are UTF-16 code units, exactly like JavaScript.

```ocaml
open Quickjs

let () =
  (* RegExp - JavaScript-compatible regular expressions *)
  let re = RegExp.compile ~flags:"g" "(?<word>\\w+)" |> Result.get_ok in
  (match RegExp.exec re "hello world" with
  | Some m ->
      assert (m.captures = [| Some "hello"; Some "hello" |]);
      assert (RegExp.group "word" m = Some "hello");
      assert (m.index = 0)
  | None -> assert false);
  (* the global flag advances last_index: the next exec finds "world" *)
  (match RegExp.exec re "hello world" with
  | Some m -> assert (RegExp.group "word" m = Some "world")
  | None -> assert false);

  (* Pathological patterns can be bounded with a timeout *)
  (try ignore (RegExp.exec ~timeout_ms:100.0 re "hello world")
   with RegExp.Timeout -> print_endline "regexp took too long");

  (* The 'd' flag records capture group positions (match.indices) *)
  let re = RegExp.compile ~flags:"d" "b(c)" |> Result.get_ok in
  (match RegExp.exec re "abcd" with
  | Some m ->
      let indices = Option.get m.indices in
      assert (indices.ranges = [| Some (1, 3); Some (2, 3) |])
  | None -> assert false);

  (* Number.Prototype - JavaScript-identical number formatting *)
  assert (Number.Prototype.to_string 0.1 = "0.1");
  (* no floating point artifacts *)
  assert (Number.Prototype.to_fixed 2 3.14159 = "3.14");
  assert (Number.Prototype.to_radix 16 255.0 = "ff");
  assert (Number.Prototype.to_exponential 2 123.456 = "1.23e+2");

  (* Global - JavaScript global functions *)
  assert (Global.parse_float "3.14" = Some 3.14);
  assert (Global.parse_int "0x10" = Some 16);
  (* Number() literal syntax, with 0x/0b/0o prefixes *)
  assert (
    Global.parse_float ~options:Global.js_number_options "0xff" = Some 255.0);

  (* Number - fast integer to string *)
  assert (Number.of_int 42 = "42");
  assert (Number.of_int_radix ~radix:16 255 = "ff");
  assert (Number.of_int64 9223372036854775807L = "9223372036854775807");

  (* String.Prototype - JavaScript string methods *)
  assert (String.Prototype.to_lower_case "HELLO" = "hello");
  assert (String.Prototype.to_upper_case "world" = "WORLD");
  assert (String.from_code_point [| 0x1F600 |] = "😀");
  assert (
    String.Prototype.split_regex "(x)?(b)" "ab"
    = [| Some "a"; None; Some "b"; Some "" |]);

  (* Unicode - case folding and the property tables behind \p{...} *)
  assert (Unicode.fold_case "Straße" = Unicode.fold_case "STRASSE");
  let greek = Option.get (Unicode.script "Greek") in
  assert (Unicode.CharSet.mem (Uchar.of_int 0x03B1) greek) (* α *)
```

For more details, see the [API reference](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/index.html).
