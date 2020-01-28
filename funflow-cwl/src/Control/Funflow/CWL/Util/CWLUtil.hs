{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : CWLUtil
Description : This module exports simple utilities for working with
 internal CWL types that arise often.

This module exports simple utilities for working with
internal CWL types that arise often.

-}


module Control.Funflow.CWL.Util.CWLUtil where

import           Data.CAS.ContentStore
    (Content (..))
import           Data.Proxy
import           Path
    (Dir, File)
import qualified Data.Scientific as Sc

import           Control.Funflow.CWL.Types.CWLTypes
import           Data.Existentials


cwlTyToProxy :: CWLType -> SomeTyProxy
cwlTyToProxy CWLStr    = SomeTyProxy (Proxy :: Proxy String)
cwlTyToProxy CWLBool   = SomeTyProxy (Proxy :: Proxy Bool)
cwlTyToProxy CWLNum = SomeTyProxy (Proxy :: Proxy Sc.Scientific)
cwlTyToProxy CWLFile   = SomeTyProxy (Proxy :: Proxy (Content File))
cwlTyToProxy CWLDir    = SomeTyProxy (Proxy :: Proxy (Content Dir))
cwlTyToProxy (CWLMaybe t) = case cwlTyToProxy t of
  SomeTyProxy (_ :: TyProxy t) -> SomeTyProxy (Proxy :: Proxy (Maybe t))
cwlTyToProxy (CWLArr t) = case cwlTyToProxy t of
  SomeTyProxy (_ :: TyProxy t) -> SomeTyProxy (Proxy :: Proxy [t])


