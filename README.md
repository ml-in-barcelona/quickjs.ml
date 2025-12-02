# quickjs

**quickjs.ml** is a set of OCaml bindings to some libraries from [QuickJS](https://quickjs-ng.github.io/quickjs/), a small and embeddable JavaScript engine that aims to support the latest ECMAScript specification.

This project exposes two libraries:

- **[`quickjs.c`](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/c/index.html)**: Low-level OCaml bindings to QuickJS C libs (`libregexp`, `libunicode`, `js_dtoa`, `js_atod`, and `cutils`)

- **[`quickjs`](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/Quickjs/index.html)**: A high-level API that mirrors JavaScript's built-in objects and methods. Modules include `RegExp`, `String`, `Number`, and `Global`.

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

For more details, see the [API reference](https://ml-in-barcelona.github.io/quickjs.ml/docs/local/quickjs/index.html).
