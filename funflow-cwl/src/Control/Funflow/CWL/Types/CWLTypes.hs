{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE StandaloneDeriving   #-}

{-|
Module      : CWLTypes
Description : This module exports the core types that abstract cwl entities.

This module exports the core types that abstract cwl entities.

-}



module Control.Funflow.CWL.Types.CWLTypes where

-- Lib Imports
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Scientific as Sc



-- * Top Level Data
-----------------------------------------------------------------

-- | A CWLFile is either a tool or a workflow.
data CWLFile where
  WfFile :: Workflow -> CWLFile
  CLToolFile :: CommandLineTool -> CWLFile
  deriving (Show, Ord, Eq)

-- | The possible values a CWL job file could have.
data CWLObject where
  CWLstr   :: String -> CWLObject
  CWLbool  :: Bool -> CWLObject
  CWLnum   :: Sc.Scientific -> CWLObject
  CWLfile  :: String -> CWLObject
  CWLdir   :: String -> CWLObject
  CWLmaybe :: (Maybe CWLObject, CWLType) -> CWLObject
  CWLarr   :: ([CWLObject], CWLType) -> CWLObject
  deriving (Eq, Ord, Show)


-- | The type of a CWL input.
data CWLType where
  CWLStr   :: CWLType
  CWLBool  :: CWLType
  CWLNum   :: CWLType
  CWLFile  :: CWLType
  CWLDir   :: CWLType
  CWLMaybe :: CWLType -> CWLType
  CWLArr   :: CWLType -> CWLType
  deriving (Show, Ord, Eq)



-- | A 'Job' gives the starting arguments for a
-- workflow or tool.
type Job = (M.HashMap T.Text CWLObject)



-- | A CWL Workflow
data Workflow where
  Workflow ::
    { w_inputs :: [InputParam]
    , w_outputs :: [WflowOutput]
    , w_class :: String
    , w_steps :: [WflowStep]
    } -> Workflow
    deriving (Show, Eq, Ord)



-- | A CWL command line tool,
-- often just called a tool.
data CommandLineTool where
  CLTool ::
    { cl_id :: String
    , cl_inputs :: [CLInput]
    , cl_outputs :: [CLOutput]
    , cl_class :: String
    , cl_stdout :: Maybe String
    , cl_args :: Maybe [Arg]
    , cl_reqs :: Maybe Reqs
    , cl_baseCmd :: [String]
    } -> CommandLineTool
    deriving (Show, Ord, Eq)





-- * Input and Output Parameter Types
-----------------------------------------------------------------

data InputParam where
  InputParam ::
    { inp_id :: String
    , inp_type :: CWLType
    } -> InputParam
    deriving (Show, Ord, Eq)


data WflowOutput where
  WflowOutput ::
    { wfout_id :: String
    , wfout_type :: CWLType
    , wfout_source :: String
    } -> WflowOutput
    deriving (Show, Ord, Eq)


data CLInput where
  CLInput ::
    { cl_input_id :: String
    , cl_input_type :: CWLType
    , cl_input_bind :: Maybe CommandLineBind
    } -> CLInput
  deriving (Show, Ord, Eq)


-- * Binding Types
--------------------------------------------------------------------------------

-- | Instructions for putting an input on the command line
-- I.e., position, prefix.
data CommandLineBind where
  CLBind ::
    { clbind_position :: Int
    , clbind_prefix :: Maybe String
    } -> CommandLineBind
  deriving (Show, Ord, Eq)

-- | Arguments in command line tool files.
data Arg where
  Arg ::
    { arg_str :: String
    , arg_pref :: Maybe String
    , arg_pos :: Int
    } -> Arg
    deriving (Eq, Ord, Show)

data CLOutput where
  CLOutput ::
    { cl_output_id :: String
    , cl_output_type :: CWLType
    , cl_output_bind :: CLOutBind   -- ^ Instructions for extracting
                                    -- the output
    } -> CLOutput
  deriving (Show, Ord, Eq)

-- | Instructions for extracting output from
-- a command line tool execution.
data CLOutBind where
  CLOutBind ::
    { clout_glob :: String
    } -> CLOutBind
  deriving (Show, Ord, Eq)



-- * Workflow Step Types
--------------------------------------------------------------------------------

data WflowStep where
  WflowStep ::
    { wfstep_id :: String
    , wfstep_input :: [WflowStepInput]
    , wfstep_output :: [WflowStepOutput]
    , wfstep_run :: String
    , wfstep_scat :: Maybe Scatter
    } -> WflowStep
    deriving (Show, Ord, Eq)


data WflowStepInput where
  WflowStepInput ::
    { wfstep_input_id :: String
    , wfstep_input_source :: String
    } -> WflowStepInput
    deriving (Show, Ord, Eq)


data WflowStepOutput where
  WflowStepOutput ::
    { wfstep_out_id :: String
    } -> WflowStepOutput
    deriving (Show, Ord, Eq)

data Scatter where
  Scatter :: [String] -> ScatterMethod -> Scatter
  deriving (Show, Ord, Eq)

data ScatterMethod where
  DotScat   :: ScatterMethod -- Dot product scatter
  CrossScat :: ScatterMethod -- Cross product scatter
  deriving (Show, Ord, Eq)

-- * Requirements
--------------------------------------------------------------------------------

-- | CWL Requirements. These are restrictions
-- on tools or workflows that specify constraints on
-- how the tool or workflow should run. As of now,
-- we only support two requirements: setting environment
-- variables and running tools in docker containers.
data Reqs where
  Reqs ::
    { envReq :: Maybe EnvReq
    , dockReq :: Maybe DockerReq
    } -> Reqs
    deriving (Eq, Ord, Show)

data EnvReq where
  EnvReq :: M.HashMap T.Text T.Text -> EnvReq
  deriving (Eq, Ord, Show)

data DockerReq where
  DockerReq ::
    { dreqImg :: String
    } -> DockerReq
  deriving (Eq, Ord, Show)




