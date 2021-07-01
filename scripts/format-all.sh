#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskell.packages.ghc883.ormolu

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
find $(dirname $DIR) -name "*.hs" -not -path "*.stack-work/*" -printf '%p\n' -exec ormolu --mode inplace {} \;