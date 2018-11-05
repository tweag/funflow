{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -Wno-unused-top-binds   #-}

{-|
Module      : Tool
Description : This module exports 'convertTool', a function to take out
  internal representation of a 'CommandLineTool' and make some Flow that
  represents that tool.

This module does its work by converting a 'CommandLineTool' into a
'FlowCmdWrapper' and using the components in a standard, intuitive way. The
result is a funflow FLow that between two records: 'SomeFlow'.
-}


module Control.Funflow.CWL.Convert.Tool ( convertTool ) where

-- Funflow
import           Control.Arrow
import           Control.Funflow
import qualified Control.Funflow.External.Docker as D

-- Not funflow
import qualified Data.Text as T
import           Path
    (File, Path, Rel)

-- Internal
import           Control.Funflow.CWL.Convert.FlowCmdWrapper
    (FlowCmdWrapper (..), mkFlowCmdWrapper)
import           Control.Funflow.CWL.Types
import           Data.HList
import           Data.ErrorMonad



convertTool :: CommandLineTool -> ErrM SomeFlow
convertTool tool = do
  flowCmdWrap <- mkFlowCmdWrapper tool
  return $ makeflow flowCmdWrap


makeflow :: FlowCmdWrapper -> SomeFlow
makeflow FlowCmdWrapper
  { fInSchema = inSch
  , fOutSchema = outSch
  , cmd = cmd'
  , paramMaker = pmaker
  , stdout = stdout'
  , extractItemOutputs = extractItemOut
  , getEnvVar = envVar'
  , dock = dock'
  } = case dock' of
    Nothing -> (SomeFlow flow inSch outSch) where
      extFn = externalFn cmd' pmaker stdout' envVar'
      flow = proc (input :: HList l) -> do
        item <- external extFn -< input
        output <- extractItemOut -< (input, item)
        returnA -< output
    Just dockReq' ->  (SomeFlow dflow inSch outSch) where
      img = dreqImg dockReq'
      dFn = dockerFn cmd' img pmaker stdout' envVar'
      dflow = proc (input :: HList l) -> do
        item <- external dFn -< input
        output <- extractItemOut -< (input, item)
        returnA -< output


-- | Given all the components for an external step, create a function from the
-- input @HList@ to @ExternalTask@ for use by the function @external@.
externalFn ::
  String                             -- ^ The command to execute.
  -> (HList l -> [Param])            -- ^ For making the other params.
  -> Maybe (Path Rel File)           -- ^ Maybe for capturing stdout.
  -> (HList l -> [(T.Text, T.Text)]) -- ^ Env Vars to Set
  -> (HList l -> ExternalTask)
externalFn cmd' mkParams stdCapture env inHList =
  let outCapture = determineStdOut stdCapture in
    ExternalTask
      { _etCommand = T.pack cmd'
      , _etParams = mkParams inHList
      , _etEnv = EnvExplicit $ map (fmap textParam) (env inHList)
      , _etWriteToStdOut = outCapture
      }

-- | This is exactly like 'externalFn' but for a docker external step.
dockerFn ::
  String                             -- ^ The command to execute.
  -> String                          -- ^ The docker image
  -> (HList l -> [Param])            -- ^ For making the other params.
  -> Maybe (Path Rel File)           -- ^ Maybe for capturing stdout.
  -> (HList l -> [(T.Text, T.Text)]) -- ^ Env Vars to Set
  -> (HList l -> ExternalTask)
dockerFn cmd' img mkParams stdCapture env inHList = D.toExternal $
  D.Config
    { D.image = T.pack img
    , D.optImageID = Nothing
    , D.command = stringParam cmd'
    , D.args = mkParams inHList
    , D.env = EnvExplicit $ map (onSnd textParam) $ env inHList
    , D.stdout = determineStdOut stdCapture
    }

determineStdOut :: Maybe (Path Rel File) -> OutputCapture
determineStdOut Nothing     = NoOutputCapture
determineStdOut (Just path) = CustomOutCapture path



-- # Lib

onSnd :: (a -> b) ->  (z,a) -> (z, b)
onSnd f (z,a) = (z, f a)




