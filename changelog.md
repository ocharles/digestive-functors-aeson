# Changelog

## 1.1.9

- Builds with lens < 4.3.

## 1.1.8

- Builds with base 4.7

## 1.1.7

- Correctly parse 'true' and 'false'. Now parses 'true' to be the string "on",
  which means that the 'bool' form provided by `digestive-functors` works as
  expected.

## 1.1.6

- Build with `lens` 4.1.

## 1.1.5

- Upgraded to `lens` 4.0. Dropped `lens-aeson` dependency.

## 1.1.4

- Upgraded to `digestive-functors` 0.7.

## 1.1.3

- This version only includes distribution changes
- The cabal file now correctly specifies the correct version of
  the digestive-functors-aeson build dependency.
- Tests are now ran using tasty rather than test-framework.

## 1.1.2

- Support top level lists. This means you can now parse the JSON document
  `[ 0, 1, 2 ]` with the form `listOf stringRead`.

## 1.1

- Support lists

-----

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
