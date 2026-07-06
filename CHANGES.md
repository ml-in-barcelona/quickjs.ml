# Changes

## 0.5.0

### RegExp

- **Breaking:** `RegExp.exec` returns `match_result option`: `None` on no match, and a record with `captures`, `index`, `input` and `groups` on match
- **Breaking:** captures are `string option array`: a group that didn't participate in the match is `None`, like `undefined` in JavaScript
- **Breaking:** `index`, `last_index` and `set_last_index` are UTF-16 code units (like JavaScript), not byte offsets
- **Breaking:** `compile` rejects invalid flags (unknown, duplicated, or `u` with `v`) instead of ignoring them. Adds support for the `d` and `v` flags
- Fix `last_index` tracking: no more wrong values, overlapping matches or infinite loops when patterns have capture groups
- Fix `exec` updating `last_index` on non-global, non-sticky regexps
- Fix memory leaks: compiled regexps and character classes no longer leak
- Fix crash when patterns are nested too deeply: returns ``Error `Stack_overflow``
- Fix `flags` reporting a `u` flag the user never passed for non-ASCII patterns
- Add `?timeout_ms` to `exec`/`test` to bound catastrophic backtracking (raises `RegExp.Timeout`)

### String

- **Breaking:** `match_` and `match_flags` return `RegExp.match_result option`; `match_all` returns `RegExp.match_result list`; `match_global` accepts `?flags`
- **Breaking:** regex-based methods raise `Invalid_argument` on invalid patterns instead of silently returning "no match"
- Fix `` $` `` in `replace_all`/`replace_regex_global`: it now refers to the original string
- Fix `split_regex` to match JavaScript on empty matches and empty input
- Fix `trim`/`trim_start`/`trim_end` to trim all JavaScript whitespace (was missing U+1680)
- Fix `to_lower_case` for the Greek final sigma: `"ΑΣ"` is `"ας"`
- Fix `starts_with_from` raising on huge positions
- Add `utf16_index_of_byte`/`byte_index_of_utf16` to convert between byte offsets and UTF-16 indices

### Global

- **Breaking:** `parse_int` auto-detects the radix like JavaScript (`parse_int "0x10"` is `Some 16`) and returns `None` for values that don't fit in an OCaml `int`
- **Breaking:** rename `js_parse_options` to `js_number_options`
- Fix `parse_float_partial`: it returned `None` for virtually every input
- Fix `parse_float "never"` returning `Some nan`; unparsable input is always `None`
- `parse_float`/`parse_float_partial`/`parse_int` skip leading whitespace like JavaScript
- Add `parse_int_float`: parseInt returning a float, exactly like JavaScript for values of any size

### Number

- Fix invalid formatting options aborting the process: they raise `Invalid_argument`
- `to_fixed`/`to_precision`/`to_exponential` use JavaScript's 0-100 digit ranges

### Libraries

- **Breaking:** `quickjs.c` and `quickjs.bindings` are wrapped: `Libregexp`, `Atod`, `Dtoa`, `Cutils` and `Bindings` become `Quickjs_c.*` and `Quickjs_bindings`

## 0.4.2

- Rename `RegExp.lastIndex` -> `RegExp.last_index`
- Rename `RegExp.setLastIndex` -> `RegExp.set_last_index`

## 0.4.1

- Remove `uutf`

## 0.4.0

- **Breaking:** Migrated from `bellard/quickjs` to `quickjs-ng/quickjs` - the actively maintained community fork with Unicode 17.0.0 support, better spec compliance, and ongoing development
- Fix arm32v7 tests on 32-bits
- Fix `Global.parse_float` for incomplete exponents ("1e", "1e+", "1e-") to return the parsed number instead of `None`, matching JavaScript spec. Workaround for [quickjs-ng/quickjs#1259](https://github.com/quickjs-ng/quickjs/issues/1259)

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
