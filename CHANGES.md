# Changes

## 0.5.0

- **Breaking:** `RegExp.exec` returns `match_result option`: `None` on no match, and a record with `captures`, `index`, `input`, `groups` and `indices` on match
- **Breaking:** RegExp captures are `string option array`: a group that didn't participate in the match is `None`, like `undefined` in JavaScript
- **Breaking:** RegExp's `index`, `last_index` and `set_last_index` are UTF-16 code units (like JavaScript), not byte offsets
- **Breaking:** `RegExp.compile` rejects invalid flags (unknown, duplicated, or `u` with `v`) instead of ignoring them. Adds support for the `d` and `v` flags
- **Breaking:** `String.Prototype.match_` and `match_flags` return `RegExp.match_result option`; `match_all` returns `RegExp.match_result list`; `match_global` accepts `?flags`
- **Breaking:** `String.Prototype` regex-based methods raise `Invalid_argument` on invalid patterns instead of silently returning "no match"
- **Breaking:** `String.Prototype.split_regex` returns `string option array`: a capture group that did not participate in the match splices `None` (JavaScript's `undefined`) instead of `""`
- **Breaking:** `Global.parse_int` auto-detects the radix like JavaScript (`parse_int "0x10"` is `Some 16`) and returns `None` for values that don't fit in an OCaml `int`
- **Breaking:** rename `Global.js_parse_options` to `Global.js_number_options`
- **Breaking:** `quickjs.c` and `quickjs.bindings` are wrapped: `Libregexp`, `Atod`, `Dtoa`, `Cutils` and `Bindings` become `Quickjs_c.*` and `Quickjs_bindings`
- Fix `RegExp.last_index` tracking: no more wrong values, overlapping matches or infinite loops when patterns have capture groups
- Fix `RegExp.exec` updating `last_index` on non-global, non-sticky regexps
- Fix memory leaks: compiled regexps and character classes no longer leak
- Fix crash when regexp patterns are nested too deeply: returns ``Error `Stack_overflow``
- Fix `RegExp.flags` reporting a `u` flag the user never passed for non-ASCII patterns
- Fix `` $` `` in `String.Prototype.replace_all`/`replace_regex_global`: it now refers to the original string
- Fix `String.Prototype.split_regex` to match JavaScript on empty matches and empty input
- Fix `String.Prototype.split_limit` to coerce its limit with ToUint32 like JavaScript: negative limits wrap to huge values and behave as "no limit" (previously they returned `[||]`)
- Fix `String.Prototype.slice`/`substring`/`substr` raising `Invalid_argument` when their range splits a surrogate pair; the lone half becomes U+FFFD (JavaScript keeps the lone surrogate, which UTF-8 cannot represent)
- Fix `String.Prototype.trim`/`trim_start`/`trim_end` to trim all JavaScript whitespace (was missing U+1680)
- Fix `String.Prototype.to_lower_case` for the Greek final sigma: `"ΑΣ"` is `"ας"`
- Fix `String.Prototype.starts_with_from` raising on huge positions
- Fix `Global.parse_float_partial`: it returned `None` for virtually every input
- Fix `Global.parse_float "never"` returning `Some nan`; unparsable input is always `None`
- Fix invalid `Number` formatting options aborting the process: they raise `Invalid_argument`
- `Global.parse_float`/`parse_float_partial`/`parse_int` skip leading whitespace like JavaScript
- `Number.Prototype.to_fixed`/`to_precision`/`to_exponential` use JavaScript's 0-100 digit ranges
- Add RegExp match indices (the `d` flag, ES2022): `match_result.indices` carries the UTF-16 `[start, end)` range of every capture group (`None` without the flag, like JavaScript's `hasIndices`), plus `group_indices` for named groups, like JavaScript's `match.indices.groups`
- Add `?timeout_ms` to `RegExp.exec`/`test` to bound catastrophic backtracking (raises `RegExp.Timeout`)
- Add `String.from_char_code` and `String.from_code_point`, JavaScript's `String.fromCharCode()`/`String.fromCodePoint()`: ToUint16 coercion, surrogate-pair composition and the RangeError contract included; unpaired surrogates become U+FFFD since UTF-8 cannot represent them
- Add `String.Prototype.split_regex_limit`, JavaScript's `split(regexp, limit)`: spliced capture groups count toward the limit
- Add `String.utf16_index_of_byte`/`byte_index_of_utf16` to convert between byte offsets and UTF-16 indices
- Add `Global.parse_int_float`: parseInt returning a float, exactly like JavaScript for values of any size
- Add `Unicode.fold_case`/`fold_case_char`: Unicode full case folding for caseless comparison, e.g. `fold_case "Straße" = "strasse"`. Folds to a fixpoint, covering the capitals QuickJS's table folds one step short (`fold_case "ẞ"` is `"ss"`, not `"ß"`)
- Add `Unicode.script`, `general_category` and `binary_property` character-set lookups with the `Unicode.CharSet` module: the Unicode property tables behind JavaScript's `\p{...}` escapes, e.g. `Unicode.script "Greek"`
- Add `Quickjs_c.Libunicode.char_range`/`char_range_free`: raw bindings to libunicode's character range tables (`unicode_script`, `unicode_general_category`, `unicode_prop`)

## 0.4.2

- Rename `RegExp.lastIndex` -> `RegExp.last_index`
- Rename `RegExp.setLastIndex` -> `RegExp.set_last_index`

## 0.4.1

- Remove `uutf`

## 0.4.0

- **Breaking:** Migrated from `bellard/quickjs` to `quickjs-ng/quickjs` - the actively maintained community fork with Unicode 17.0.0 support, better spec compliance, and ongoing development
- Fix `Global.parse_float` for incomplete exponents ("1e", "1e+", "1e-") to return the parsed number instead of `None`, matching JavaScript spec. Workaround for [quickjs-ng/quickjs#1259](https://github.com/quickjs-ng/quickjs/issues/1259)

## 0.3.0

- Fix openSUSE
- **Breaking:** Reorganized API to mirror JavaScript built-in objects for `quickjs`
  - `Dtoa` → `Number.Prototype` (toString, toFixed, toPrecision, toExponential, toRadix)
  - `Atod` → `Global` (parse_float, parse_float_partial)
  - `IntToString` → `Number` (of_int, of_int32, of_int64, of_int_radix, etc.)
  - New `String.Prototype` module (to_lower_case, to_upper_case, normalize)
  - New `Unicode` module for character-level operations (is_cased, is_id_start, canonicalize, etc.)
- Fix String.Prototype.lastIndexOf
- Fix String.Prototype.match

## 0.2.0

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
