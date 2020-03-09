{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pretty-printer for Funflow diagrams.
module Control.Funflow.Pretty where

import           Control.Funflow.Base
import           Control.Funflow.Diagram
import qualified Data.Text               as T
import           Text.PrettyPrint

ppFlow :: Flow eff ex a b -> Doc
ppFlow = ppDiagram . toDiagram where
  ppDiagram :: forall ex a b. Diagram ex a b -> Doc
  ppDiagram (Node (NodeProperties (lbl:_)) _ _)    = text . T.unpack $ lbl
  ppDiagram (Node _ _ _)    = text "unlabeled step"
  ppDiagram (Seq f g) = parens $ ppDiagram f <+> text ">>>" <+> ppDiagram g
  ppDiagram (Par f g) = parens $ ppDiagram f <+>  text "***" <+> ppDiagram g
  ppDiagram (Fanin f g) = parens $ ppDiagram f <+>  text "|||" <+> ppDiagram g
  ppDiagram (Try f) = parens $ text "try" <+> ppDiagram f
  ppDiagram (Traverse f) = parens $ text "traverse" <+> ppDiagram f
  ppDiagram (Wander _trav f) = parens $ text "wander" <+> ppDiagram f

showFlow :: Flow eff ex a b -> String
showFlow = render . ppFlow
