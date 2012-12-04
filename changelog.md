# Changelog

## 1.0.3

- Added support for `listOf` to consume JSON arrays.

## 1.0.2

- Added `jsonErrors`, which can transform a `ToJSON a => View a` into a aeson
  `Value`. This respects the validation hierarchy.
- Tests!

-----

## 1.0.1

- Added a dependency on the `Safe` package to build with GHC < 7.6.

-----

## 1.0.0

- Initial release
