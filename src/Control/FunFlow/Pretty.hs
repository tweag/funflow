{-# LANGUAGE Arrows, GADTs, OverloadedStrings #-}

module Control.FunFlow.Pretty where

import Text.PrettyPrint
import Control.FunFlow.Base
import qualified Data.Text as T

ppFlow :: Flow a b -> Doc
ppFlow (Step _) = text "step"
ppFlow (Name nm f) = text (T.unpack nm) <> parens (ppFlow f)
ppFlow (Arr _) = text "arr"
ppFlow (Compose f g) = parens $ ppFlow f <+>  text ">>>" <+> ppFlow g
ppFlow (Par f g) = parens $ ppFlow f <+>  char '|' <+> ppFlow g
ppFlow (Fanin f g) = parens $ ppFlow f <+>  text "|||" <+> ppFlow g
ppFlow (First f) = text "first" <+> parens (ppFlow f)

showFlow :: Flow a b -> String
showFlow = render . ppFlow