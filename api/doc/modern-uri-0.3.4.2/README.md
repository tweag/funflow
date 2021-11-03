# Modern URI

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/modern-uri.svg?style=flat)](https://hackage.haskell.org/package/modern-uri)
[![Stackage Nightly](http://stackage.org/package/modern-uri/badge/nightly)](http://stackage.org/nightly/package/modern-uri)
[![Stackage LTS](http://stackage.org/package/modern-uri/badge/lts)](http://stackage.org/lts/package/modern-uri)
![CI](https://github.com/mrkkrp/modern-uri/workflows/CI/badge.svg?branch=master)

This is a modern library for working with URIs in Haskell as per RFC 3986:

https://tools.ietf.org/html/rfc3986

## Features

The `modern-uri` package features:

* Correct by construction `URI` data type. The correctness is ensured by
  making sure that every sub-component of the `URI` record cannot be
  invalid.
* Textual components in the `URI` data type are represented as `Text` rather
  than `ByteString`, because they are percent-decoded and so they can
  contain characters outside of ASCII range (i.e. Unicode). This allows for
  easier manipulation of `URI`s, while encoding and decoding headaches are
  handled by the parsers and renders for you.
* Absolute and relative URIs differ only by the scheme component: if it's
  `Nothing`, then the URI is relative, otherwise it's absolute.
* A Megaparsec parser that can be used as a standalone smart constructor for
  the `URI` data type (see `mkURI`) as well as seamlessly integrated into a
  bigger Megaparsec parser that consumes a strict `Text` (see `parser`) or
  strict `ByteString` (see `parserBs`).
* The parser performs some normalization, for example it collapses
  consecutive slashes. Some smart constructors such as `mkScheme` and
  `mkHost` also perform normalization. So in a sense URIs are also
  “normalized by construction” to some extent.
* Fast rendering to strict `Text` and `ByteString` as well as to their
  respective `Builder` types and to `String`/`ShowS`.
* Extensive set of lensy helpers for easier manipulation of the nested data
  types (see `Text.URI.Lens`).
* Quasi-quoters for compile-time construction of the `URI` data type and
  refined text types (see `Text.URI.QQ`).

## Quick start

The `modern-uri` package serves three main purposes:

* Construction of the `URI` data type.
* Inspection and manipulation of the `URI` data type (in the sense of
  changing its parts).
* Rendering of `URI`s.

Let's walk through every operation quickly.

### Construction of `URI`s

There are four ways to create a `URI` value. First off, one could assemble
it manually like so:

```haskell
λ> :set -XOverloadedStrings
λ> import qualified Text.URI as URI
λ> scheme <- URI.mkScheme "https"
λ> scheme
"https"
λ> host <- URI.mkHost "markkarpov.com"
λ> host
"markkarpov.com"
λ> let uri = URI.URI (Just scheme) (Right (URI.Authority Nothing host Nothing)) Nothing [] Nothing
λ> uri
URI
  { uriScheme = Just "https",
    uriAuthority =
      Right
        ( Authority
            { authUserInfo = Nothing,
              authHost = "markkarpov.com",
              authPort = Nothing
            }
        ),
    uriPath = Nothing,
    uriQuery = [],
    uriFragment = Nothing
  }
```

In this library we use quite a few refined text values. They only can be
constructed by using smart constructors like `mkScheme :: MonadThrow m =>
Text -> m (RText 'Scheme)`. For example, if argument to `mkScheme` is not a
valid scheme, an exception will be thrown. Note that monads such as `Maybe`
are also instances of the `MonadThrow` type class, and so the smart
constructors can be used in pure environment as well.

There is a smart constructor that can make an entire `URI` too, it's called
(unsurprisingly) `mkURI`:

```haskell
λ> uri <- URI.mkURI "https://markkarpov.com"
λ> uri
URI
  { uriScheme = Just "https",
    uriAuthority =
      Right
        ( Authority
            { authUserInfo = Nothing,
              authHost = "markkarpov.com",
              authPort = Nothing
            }
        ),
    uriPath = Nothing,
    uriQuery = [],
    uriFragment = Nothing
  }
```

If the argument of `mkURI` is not a valid URI, then an exception will be
thrown. The exception will contain full context and the actual parse error.

If some refined text value or `URI` is known statically at compile time, we
can use Template Haskell, namely the “quasi quotes” feature. To do so import
the `Text.URI.QQ` module and enable the `QuasiQuotes` language extension,
like so:

```haskell
λ> :set -XQuasiQuotes
λ> import qualified Text.URI.QQ as QQ
λ> let uri = [QQ.uri|https://markkarpov.com|]
λ> uri
URI
  { uriScheme = Just "https",
    uriAuthority =
      Right
        ( Authority
            { authUserInfo = Nothing,
              authHost = "markkarpov.com",
              authPort = Nothing
            }
        ),
    uriPath = Nothing,
    uriQuery = [],
    uriFragment = Nothing
  }
```

Note how the value returned by the `url` quasi quote is pure, its
construction cannot fail because when there is an invalid URI inside the
quote it's a compilation error. The `Text.URI.QQ` module has quasi-quoters
for scheme, host, and other components.

Finally, the package provides two Megaparsec parsers: `parser` and
`parserBs`. The first works on strict `Text`, while the other one works on
strict `ByteString`s. You can use the parsers in a bigger Megaparsec parser
to parse `URI`s.

### Inspection and manipulation

Although one could use record syntax directly, possibly with language
extensions like `RecordWildcards`, the best way to inspect and edit parts of
`URI` is with lenses. The lenses can be found in the `Text.URI.Lens` module.
If you have never used the
[`lens`](https://hackage.haskell.org/package/lens) library, you could
probably start by reading/watching materials suggested in the library
description on Hackage.

Here are some examples, just to show off what you can do:

```haskell
λ> import Text.URI.Lens
λ> uri <- URI.mkURI "https://example.com/some/path?foo=bar&baz=quux&foo=foo"
λ> uri ^. uriScheme
Just "https"
λ> uri ^? uriAuthority . _Right . authHost
Just "example.com"
λ> uri ^. isPathAbsolute
True
λ> uri ^. uriPath
["some","path"]
λ> k <- URI.mkQueryKey "foo"
λ> uri ^.. uriQuery . queryParam k
["bar","foo"]
-- etc.
```

### Rendering

Rendering turns a `URI` into a sequence of bytes or characters. Currently
the following options are available:

* `render` for rendering to strict `Text`.
* `render'` for rendering to text `Builder`. It's possible to turn that into
  lazy `Text` by using the `toLazyText` function from
  `Data.Text.Lazy.Builder`.
* `renderBs` for rendering to strict `ByteString`.
* `renderBs'` for rendering to byte string `Builder`. Similarly it's
  possible to get a lazy `ByteString` from that by using the
  `toLazyByteString` function from `Data.ByteString.Builder`.
* `renderStr` can be used to render to `String`. Sometimes it's handy. The
  render uses difference lists internally so it's not that slow, but in
  general I'd advise avoiding `String`s.
* `renderStr'` returns `ShowS`, which is just a synonym for `String ->
  String`—a function that prepends the result of rendering to a given
  `String`. This is useful when the `URI` you want to render is a part of a
  bigger output, just like with the builders mentioned above.

Examples:

```haskell
λ> uri <- mkURI "https://markkarpov.com/posts.html"
λ> render uri
"https://markkarpov.com/posts.html"
λ> renderBs uri
"https://markkarpov.com/posts.html"
λ> renderStr uri
"https://markkarpov.com/posts.html"
-- etc.
```

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/modern-uri/issues).

Pull requests are also welcome.

## License

Copyright © 2017–present Mark Karpov

Distributed under BSD 3 clause license.
