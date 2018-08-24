{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds   #-}



{-|
Module      : PreProcess
Description : This module exports one function for preprocessing
  cwl files and job files. As of now, it does no processing and
  simply parses.

This module exports one function for preprocessing
cwl files and job files. As of now, it does no processing and
simply parses.

-}


module Control.Funflow.CWL.PreProcess ( preProcess ) where

import qualified Data.HashMap.Strict as M
import Data.Yaml

import Control.Funflow.CWL.Types
import Control.Funflow.CWL.Parse ()
import Data.ErrorMonad

-- * Top Level Function
--------------------------------------------------------------------------------


-- | This pre-processes the cwl file and job according
-- to a subset of the salad specification and for some of
-- my needs. The first filepath is the cwl file and the
-- second is the job.
preProcess :: FilePath -> FilePath -> IOErrM (Value, Value)
preProcess f j = do
  cwlfileObj <- parseToObj f
  jobObj <- parseToObj j
  return (Object cwlfileObj, Object jobObj)


parseToObj :: FilePath -> IOErrM Object
parseToObj path = do
  value <- parseToJSON path
  case value of
    Object obj -> return obj
    _ ->
      throwM $ ErrMsg $
      "This file is not a yaml object: " ++ path

  where

    parseToJSON :: FilePath -> IOErrM Value
    parseToJSON = injectIOEither . decodeFileEither




-- * Scaffolding for future work
--------------------------------------------------------------------------------

getArrTy :: CWLType -> Maybe CWLType
getArrTy (CWLArr ty) = Just ty
getArrTy _ = Nothing

getMaybeTy :: CWLType -> Maybe CWLType
getMaybeTy (CWLMaybe ty) = Just ty
getMaybeTy _ = Nothing


-- | Given the object for a cwlwf or tool,
-- determine the input schema.
getInSchema :: Object -> ErrM (M.HashMap String CWLType)
getInSchema = parseMonad parseInSchema

parseInSchema :: Object -> Parser (M.HashMap String CWLType)
parseInSchema obj = do
  class' :: String <- obj .: "class"
  case class' of
    "Workflow" -> do
      inparams :: [InputParam] <- obj .: "inputs"
      return $ M.fromList $ ipToAssocList inparams
    "CommandLineTool" -> do
      clins :: [CLInput] <- obj .: "inputs"
      return $ M.fromList $ clinToAssocList clins
    _ -> fail "File not a Tool or Workflow."

  where

    ipToAssocList :: [InputParam] -> [(String, CWLType)]
    ipToAssocList = map getPair where
      getPair (InputParam id' ty) = (id',ty)

    clinToAssocList :: [CLInput] -> [(String, CWLType)]
    clinToAssocList = map getPair where
      getPair (CLInput id' ty _) = (id',ty)









