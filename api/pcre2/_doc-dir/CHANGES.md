# Changelog

## 8.0.3 (2025-02-15)

- ugh: forgot to make the function caml_alloc_some (for ocaml [4.08, 4.11] support) static.  It clashes with the same function from the package `pcre`

## 8.0.2 (2024-12-26)

- Thanks to @nojb, try to get it working for ocaml [4.08, 4.11]

## 8.0.1 (2024-12-20)

- Merged all changes from old `pcre-ocaml`.

- Fixed a bug in the `full_split` function where non-capturing groups were
  not identified as such.

## 7.5.3 (2024-12-23)

* @mmottl fixed bug in `full_split`

## 7.5.2 (2023-09-06)

- Fixed bug in `full_split`, added first unit-test for same

## 7.5.1 (2023-09-01)

- Created pcre2-ocaml bindings based on original pcre-ocaml project
