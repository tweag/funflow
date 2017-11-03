{-# LANGUAGE Arrows              #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.FunFlow.Pretty where

import           Control.FunFlow.Base
import           Control.FunFlow.Diagram
import qualified Data.Text               as T
import           Text.PrettyPrint

ppFlow :: Flow ex a b -> Doc
ppFlow = ppDiagram . toDiagram where
  ppDiagram :: forall ex a b. Diagram ex a b -> Doc
  ppDiagram (Node (NodeProperties (lbl:_)) _ _)    = text . T.unpack $ lbl
  ppDiagram (Node _ _ _)    = text "unlabeled step"
  ppDiagram (Seq f g) = parens $ ppDiagram f <+> text ">>>" <+> ppDiagram g
  ppDiagram (Par f g) = parens $ ppDiagram f <+>  text "***" <+> ppDiagram g
  ppDiagram (Fanin f g) = parens $ ppDiagram f <+>  text "|||" <+> ppDiagram g
  ppDiagram (Catch f g) = parens $ ppDiagram f <+>  text "catch" <+> ppDiagram g

showFlow :: Flow ex a b -> String
showFlow = render . ppFlow
