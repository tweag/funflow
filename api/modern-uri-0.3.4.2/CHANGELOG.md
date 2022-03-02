## Modern URI 0.3.4.2

* Improved handling of percent-encoded sequences of bytes that cannot be
  decoded as UTF-8 text. Now friendly error messages are reported in these
  cases.

## Modern URI 0.3.4.1

* Works with GHC 9.0.1.

## Modern URI 0.3.4.0

* URIs with authority component and without path are now rendered without
  trailing slashes.

## Modern URI 0.3.3.1

* Works with `bytestring-0.11`.

## Modern URI 0.3.3.0

* Added `mkURIBs` for parsing `ByteString` as a `URI`.

## Modern URI 0.3.2.0

* Quasi-quoters from `Text.URI.QQ` now can be used in pattern context when
  the `ViewPatterns` extension is enabled.

* Dropped support for GHC 8.2.x.

## Modern URI 0.3.1.0

* Dropped support for GHC 8.0 and 7.10.

* Added Template Haskell `Lift` instance for the `URI` type and its
  sub-components.

## Modern URI 0.3.0.1

* Allow superfluous `&` right after question sign in query parameters.

## Modern URI 0.3.0.0

* Uses Megaparsec 7. Visible API changes amount to an adjustment in
  definition of the `ParseException` type.

## Modern URI 0.2.2.0

* Removed a potentially overlapping instance `Arbitrary (NonEmpty (RText
  'PathPiece))`.

* Fixed a bug that made it impossible to have empty host names. This allows
  us to parse URIs like `file:///etc/hosts`.

## Modern URI 0.2.1.0

* Added `emptyURI`—`URI` value representing the empty URI.

## Modern URI 0.2.0.0

* Changed the type of `uriPath` field of the `URI` record from `[RText
  'PathPiece]` to `Maybe (Bool, NonEmpty (RText 'PathPiece))`. This allows
  us to store whether there is a trailing slash in the path or not. See the
  updated documentation for more information.

* Added the `relativeTo` function.

* Added the `uriTrailingSlash` 0-1 traversal in `Text.URI.Lens`.

## Modern URI 0.1.2.1

* Allow Megaparsec 6.4.0.

## Modern URI 0.1.2.0

* Fixed handling of `+` in query strings. Now `+` is parsed as space and
  serialized as `%2b` as per RFC 1866 (paragraph 8.2.1). White space in
  query parameters is serialized as `+`.

## Modern URI 0.1.1.1

* Fixed implementation of `Text.URI.Lens.queryParam` traversal.

## Modern URI 0.1.1.0

* Derived `NFData` for `ParseException`.

* Adjusted percent-encoding in renders so it's only used when absolutely
  necessary. Previously we percent-escaped a bit too much, which, strictly
  speaking, did not make the renders incorrect, but that didn't look nice
  either.

## Modern URI 0.1.0.1

* Updated the readme to include “Quick start” instructions and some
  examples.

## Modern URI 0.1.0.0

* Changed the type of `uriAuthority` from `Maybe Authority` to `Either Bool
  Authority`. This allows to know if URI path is absolute or not without
  duplication of information, i.e. when the `Authority` component is present
  the path is necessarily absolute, otherwise the `Bool` value tells if it's
  absolute (`True`) or relative (`False`).

* Added `isPathAbsolute` in `Text.URI` and the corresponding getter in
  `Text.URI.Lens`.

## Modern URI 0.0.2.0

* Added the `renderStr` and `renderStr'` functions for efficient rendering
  to `String` and `ShowS`.

* Added the `parserBs` that can consume strict `ByteString` streams.

## Modern URI 0.0.1.0

* Initial release.
