# Changes

## 0.5.0

Complete overhaul of the RegExp/Global/Number/Unicode layers following a
full-codebase audit. Many changes are breaking; the API is now smaller,
typed, and consistently UTF-16-indexed like JavaScript.

### RegExp (breaking)

- **Breaking:** `RegExp.exec` now returns `match_result option` instead of an
  opaque `result`. `match_result` is a plain record with `captures`,
  `index`, `input` and `groups` fields; no match is `None`, so no-match,
  error and empty-match can no longer be confused
- **Breaking:** captures are `string option array`: capture groups that did
  not participate in a match are `None` (JavaScript's `undefined`),
  distinct from groups that matched the empty string
- **Breaking:** all indices (`index`, `last_index`, `set_last_index`) are
  UTF-16 code units, matching JavaScript and `String.Prototype`. They were
  previously UTF-8 byte offsets, which corrupted every `String` regex helper
  on non-ASCII input
- Fixed `last_index` being overwritten by every capture group: it is now
  derived from the full match only. This fixes overlapping re-matches,
  infinite loops in `match_global`/`match_all`/`replace_regex_global` with
  zero-width groups, and garbage `last_index` values (raw pointer
  arithmetic) when the last group did not participate
- Fixed `exec` mutating `last_index` on non-global, non-sticky regexps
- `compile` now validates flags like JavaScript: unknown flags, duplicated
  flags and combining `u` with `v` return ``Error (`Invalid_flags _)``. The
  `d` (indices) and `v` (unicodeSets) flags are supported
- Non-unicode patterns containing astral code points are compiled as UTF-16
  surrogate pairs (CESU-8), exactly like JavaScript, instead of silently
  forcing the `u` flag; `flags` now always returns exactly what was passed
- Fixed two memory leaks: compiled bytecode was never freed (it is now
  copied into GC-accounted storage and the C buffer released immediately,
  so memory stays bounded under natural GC pressure), and the `lre_realloc`
  shim leaked one allocation per character class per compile
  (`realloc(NULL, 0)` allocates on glibc; the embedder callback must treat
  size 0 as free)
- `set_last_index` clamps negative values to 0 instead of passing them to C
  (out-of-bounds reads)
- Added `?timeout_ms` to `exec`/`test` (raises `RegExp.Timeout`), bounding
  catastrophic backtracking; the C-stack guard now makes deeply nested
  patterns fail with ``Error `Stack_overflow`` instead of crashing
- UTF-16 buffers use the platform's endianness (fixes big-endian, e.g.
  s390x; the test suite is no longer disabled there)

### String (breaking)

- **Breaking:** `match_` and `match_flags` return `RegExp.match_result
  option`; `match_all` returns `RegExp.match_result list` (its private
  result type is gone); `match_global` gained `?flags`. `match_flags`
  rejects the `g` flag (use `match_global`/`match_all`)
- **Breaking:** regex-based methods raise `Invalid_argument` on invalid
  patterns (like JavaScript's SyntaxError) instead of silently behaving as
  "no match"
- Fixed `replace_all`/`replace_regex_global`: `` $` `` now refers to the
  original string instead of already-replaced text
- Fixed `split_regex` to follow the spec's SplitMatch semantics (empty
  matches, empty input, matches at the end of the string)
- `trim`/`trim_start`/`trim_end` use libunicode's whitespace predicate (the
  one QuickJS itself uses), fixing U+1680 and removing the duplicated
  hand-rolled whitespace list
- `to_lower_case` implements the context-sensitive Greek final sigma rule
  (`"ΑΣ"` → `"ας"`), like `String.prototype.toLowerCase()`
- Fixed integer overflow in `starts_with_from` with huge positions

### Global (breaking)

- Fixed `parse_float_partial`: it previously compared pointers of two
  unrelated string copies and returned `None` for virtually every input
- `parse_float`/`parse_float_partial`/`parse_int` skip leading JavaScript
  whitespace (including Unicode whitespace), per spec
- `parse_float` returns `None` for any unparsable input: the undocumented
  `Some nan` for strings starting with `n`/`N` is gone
- **Breaking:** `parse_int` defaults to radix auto-detection like JavaScript
  (`parse_int "0x10"` is `Some 16`); it no longer accepts `0b`/`0o`
  (JavaScript's parseInt never did); values that do not fit in an OCaml
  `int` return `None` instead of a corrupted number
- **Breaking:** renamed `js_parse_options` to `js_number_options` (it
  implements `Number()` literal syntax, not parseFloat)

### Number

- Formatting options are fully validated before reaching C:
  `Fixed 0` and radix ≠ 10 with non-`Free` formats raise `Invalid_argument`
  instead of aborting the process via a C assertion
- `to_fixed`/`to_precision`/`to_exponential` enforce JavaScript's 0–100
  digit ranges with accurate error messages

### Libraries

- **Breaking:** `quickjs.c` and `quickjs.bindings` are wrapped: modules are
  now `Quickjs_c.Libregexp`, `Quickjs_c.Atod`, etc., instead of claiming
  the global `Libregexp`/`Atod`/`Dtoa`/`Cutils`/`Bindings` namespaces

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
