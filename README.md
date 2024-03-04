# quickjs

**quickjs.ml** is a set of OCaml bindings for [QuickJS](https://bellard.org/quickjs). QuickJS is a small and embeddable JavaScript engine. It supports the ES2020 specification including modules, asynchronous generators, proxies and BigInt.

The project exposes two libraries:

- **`quickjs.bindings`** with the QuickJS C API

- **`quickjs`** which exposes a polished API on top of `quickjs.bindings` with the same shape as the JavaScript API

### Motivation

The purpose of this project is to provide the same behaviour as the JavaScript engines from browsers ([SpiderMonkey](https://spidermonkey.dev), [JavaScriptCore](https://developer.apple.com/documentation/javascriptcore), [ChakraCore](https://github.com/chakra-core/ChakraCore), [v8](https://v8.dev/)) into OCaml. So code that runs in the browser (via [Melange](https://melange.re)) can be run in native with the same results.

### Documentation

[API reference](https://ml-in-barcelona.github.io/quickjs.ml/quickjs/index.html)

### Status

This is a work in progress, and currently only includes bindings to `RegExp` (binded to `libregexp`) check the [`to bind` issues](https://github.com/ml-in-barcelona/quickjs.ml/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc+label%3A%22to+bind%22) from the issue tracker.
