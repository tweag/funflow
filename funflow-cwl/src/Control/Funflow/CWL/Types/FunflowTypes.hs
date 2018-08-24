{-# LANGUAGE StrictData           #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE RankNTypes           #-}


{-|
Module      : FunflowTypes
Description : This module exports an existential type for the
 flows we care about: flows between two records.

This module exports an existential type for the
Flows we care about: flows between two records

-}


module Control.Funflow.CWL.Types.FunflowTypes
  ( SomeFlow (..)
  , flowInProxy
  , flowOutProxy
  , type (==>)
  )
where

-- Funflow Imports
import Control.Funflow

-- Internal Imports
import Control.Funflow.CWL.Types.Schema
import Data.HList


-- | An existential type for a Funflow flow between two row types.
-- This is represented as a flow between two heterogeneous lists
-- where we store a list of ids for the input and output lists.
data SomeFlow where
  SomeFlow :: (TraversibleL l, TraversibleL l') =>
    { theFlow :: HList l ==> HList l'
    , inSchema :: Schema l
    , outSchema :: Schema l'
    } -> SomeFlow


instance Show SomeFlow where
  show (SomeFlow flow (Schema _ inIds) (Schema _ outIds)) =
    "{\n" ++
    "IDS::\n" ++
    "\t(" ++ show inIds ++ ")\n" ++
    "==>\n" ++
    "\t(" ++ show outIds ++ ")\n" ++
    "TYPES::\n" ++
    "\t(" ++ (show . flowInProxy $ flow) ++ ")\n" ++
    "==>\n" ++
    "\t(" ++ (show . flowOutProxy $ flow) ++ ")\n" ++
    "\n}"

flowInProxy :: HList l ==> HList l' -> ProxyList l
flowInProxy _ = ProxyList

flowOutProxy :: HList l ==> HList l' -> ProxyList l'
flowOutProxy _ = ProxyList





