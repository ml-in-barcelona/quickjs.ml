# Changes

## 0.3.0

- Fix openSUSE
- **Breaking:** Reorganized API to mirror JavaScript built-in objects for `quickjs`
  - `Dtoa` → `Number.Prototype` (toString, toFixed, toPrecision, toExponential, toRadix)
  - `Atod` → `Global` (parse_float, parse_float_partial)
  - `IntToString` → `Number` (of_int, of_int32, of_int64, of_int_radix, etc.)
  - New `String.Prototype` module (to_lower_case, to_upper_case, normalize)
  - New `Unicode` module for character-level operations (is_cased, is_id_start, canonicalize, etc.)
- Added TC39/test262 test coverage for Number, String, Global, and Unicode modules
- Added tests for special values (NaN, Infinity) in Number.Prototype methods
- Fix String.Prototype.lastIndexOf
- Fix String.Prototype.match

## 0.2.0

- Full tests coverage from Official ECMAScript Conformance Test Suite for RegExp
- Support unicode in RegExp
- Support named groups in RegExp
- Bind to dtoa (Dtoa, Atod, IntToString)

## 0.1.2

- Support for named groups
- Change `RegExp.exec` interface to return `result`

## 0.1.1

Bug fix for RegExp backtracking when not find a match

## 0.1.0

Initial release of quickjs. This release only includes bindings to libregexp and exposes the API of the QuickJS C library with the same shape as the JavaScript API: RegExp.
