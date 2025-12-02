# quickjs

**quickjs.ml** is a set of OCaml bindings to some libraries from [QuickJS](https://quickjs-ng.github.io/quickjs/), a small and embeddable JavaScript engine that aims to support the latest ECMAScript specification.

This project exposes two libraries:

- **`quickjs.c`**: Low-level OCaml bindings to QuickJS C functions (`libregexp`, `libunicode`, `js_dtoa`, `js_atod`, and `cutils`)

- **`quickjs`**: A high-level API that mirrors JavaScript's built-in objects and methods. Modules include `RegExp`, `String`, `Number`, and `Global`.

### Motivation

The purpose of this project is to provide the same behaviour as the JavaScript engines from browsers ([SpiderMonkey](https://spidermonkey.dev), [JavaScriptCore](https://developer.apple.com/documentation/javascriptcore), [ChakraCore](https://github.com/chakra-core/ChakraCore), [v8](https://v8.dev/)) into native OCaml. So code that runs in the browser (via [Melange](https://melange.re)) can be run in native with the same results.

### TC39/test262 Compatibility

We are translating [TC39/test262](https://github.com/tc39/test262) tests into OCaml to ensure full compatibility with the ECMAScript specification. This allows us to verify that our implementations behave exactly as expected by the JavaScript standard, guaranteeing consistent behaviour between browser engines and native OCaml.

### Documentation

[API reference](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/index.html)

### Usage

```ocaml
open Quickjs

(* RegExp - JavaScript-compatible regular expressions *)
let re = RegExp.compile ~flags:"g" "(?<word>\\w+)" |> Result.get_ok in
let result = RegExp.exec re "hello world" in
RegExp.captures result (* [| "hello"; "hello" |] *)
RegExp.group "word" result (* Some "hello" *)
RegExp.exec re "hello world" (* next match: "world" *)

(* Number.Prototype - JavaScript-identical number formatting *)
Number.Prototype.to_string 0.1 (* "0.1" - no floating point artifacts *)
Number.Prototype.to_fixed 2 3.14159 (* "3.14" *)
Number.Prototype.to_radix 16 255.0 (* "ff" *)
Number.Prototype.to_exponential 2 123.456 (* "1.23e+2" *)

(* Global - JavaScript global functions *)
Global.parse_float "3.14" (* Some 3.14 *)
Global.parse_float ~options:Global.js_parse_options "0xff" (* Some 255.0 *)

(* Number - fast integer to string *)
Number.of_int 42 (* "42" *)
Number.of_int_radix ~radix:16 255 (* "ff" *)
Number.of_int64 9223372036854775807L (* "9223372036854775807" *)

(* String.Prototype - JavaScript string methods *)
String.Prototype.to_lower_case "HELLO" (* "hello" *)
String.Prototype.to_upper_case "world" (* "WORLD" *)
```
