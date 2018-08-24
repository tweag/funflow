{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs                #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-|
Module      : Parse
Description : This module exports functions for parsing files
  into internal data types for CWL entities.

This module provides parsers for 'CWLFile', 'Workflow' and 'CommandLineTool'.
It uses the yaml package to do this parsing by implementing several
'FromJSON' instances.

-}


module Control.Funflow.CWL.Parse
  ( parseCWLFile
  , parseJob
  , parseWflow
  , parseTool
  )
where

import qualified Data.List.Index as I
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Yaml
    ( FromJSON (..)
    , ParseException (..)
    , Parser
    , Value (..)
    , decodeFileEither
    , (.:)
    , (.:?)
    , Array
    , parseMaybe
    )

import           Control.Funflow.CWL.Types
import Data.ErrorMonad


-- * Interface
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


parseCWLFile :: String -> IOErrM CWLFile
parseCWLFile = injectIOEither . decodeCWLFile
  where
    decodeCWLFile :: FilePath -> IO (Either ParseException CWLFile)
    decodeCWLFile = decodeFileEither

parseWflow :: FilePath ->  IOErrM Workflow
parseWflow = injectIOEither . decodeWflow
  where
    decodeWflow :: FilePath -> IO (Either ParseException Workflow)
    decodeWflow = decodeFileEither

parseTool :: FilePath -> IOErrM CommandLineTool
parseTool = injectIOEither . decodeCLTool
  where
    decodeCLTool :: FilePath -> IO (Either ParseException CommandLineTool)
    decodeCLTool = decodeFileEither

parseJob :: FilePath -> IOErrM Job
parseJob = injectIOEither . decodeJob
  where
    decodeJob :: FilePath -> IO (Either ParseException Job)
    decodeJob = decodeFileEither




-- * FromJSON Instances
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance FromJSON CWLFile where
  parseJSON :: Value -> Parser CWLFile
  parseJSON object@(Object obj) = do
    (class' :: String) <- obj .: "class"
    case class' of
      "CommandLineTool" -> do
        (tool :: CommandLineTool) <- parseJSON object
        return $ CLToolFile tool
      "Workflow" -> do
        (wf :: Workflow) <- parseJSON object
        return $ WfFile wf
      _ -> fail "The class is not a cwltool or workflow"
  parseJSON _ = fail "File is not a valid YAML object."


instance FromJSON CWLType where
  parseJSON :: Value -> Parser CWLType
  parseJSON (String text) =
    case T.unpack text of
      "File"        -> return CWLFile
      "File?"       -> return $ CWLMaybe CWLFile
      "File[]"      -> return $ CWLArr CWLFile
      "Directory"   -> return CWLDir
      "Directory?"  -> return $ CWLMaybe CWLDir
      "Directory[]" -> return $ CWLArr CWLDir
      "string"      -> return CWLStr
      "string?"     -> return $ CWLMaybe CWLStr
      "string[]"    -> return $ CWLArr CWLStr
      "boolean"     -> return CWLBool
      "boolean?"    -> return $ CWLMaybe CWLBool
      "boolean[]"   -> return $ CWLArr CWLBool
      "int"         -> return CWLNum
      "int?"        -> return $ CWLMaybe CWLNum
      "int[]"       -> return $ CWLArr CWLNum
      "long"        -> return CWLNum
      "long?"       -> return $ CWLMaybe CWLNum
      "long[]"      -> return $ CWLArr CWLNum
      "float"       -> return CWLNum
      "float?"      -> return $ CWLMaybe CWLNum
      "float[]"     -> return $ CWLArr CWLNum
      "double"      -> return CWLNum
      "double?"     -> return $ CWLMaybe CWLNum
      "double[]"    -> return $ CWLArr CWLNum
      str           -> fail ("Can't parse into CWL Type: " ++ str)
  parseJSON x@(Object obj) = do
    (ty :: String)     <- obj .: "type"
    (items :: CWLType) <- obj .: "items"
    case ty of
      "array" -> return $ CWLArr items
      "maybe" -> return $ CWLMaybe items
      _ -> fail $ "Cannot parse cwl type: " ++ show x
  parseJSON _ = fail "Expected a yaml string."


instance FromJSON CWLObject where
  parseJSON :: Value -> Parser CWLObject
  parseJSON (String text) = return $ CWLstr $ T.unpack text
  parseJSON (Bool bool)   = return $ CWLbool bool
  parseJSON (Object obj)  = do
    (class' :: String) <- obj .: "class"
    case class' of
      "maybe" -> do
        (ty :: CWLType) <-  obj .: "items"
        (item :: Maybe CWLObject) <- obj .:? "value"
        return $ CWLmaybe (item, ty)
      "array" -> do
        (ty :: CWLType) <- obj .: "items"
        (item :: [CWLObject]) <- obj .: "value"
        return $ CWLarr (item, ty)
      "File" -> do
        (path :: String) <- obj .: "path"
        return $ CWLfile path
      "Directory" -> do
        (path :: String) <- obj .: "path"
        return $ CWLdir path
      _ -> fail $
           "Can't parse CWL object. Expected \
           \'class: (File|Directory|array|maybe)'."
  parseJSON (Number s) = return $ CWLnum s
  parseJSON _ = fail "Can't parse CWL object."



-- ** Command Line Tool
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance FromJSON CommandLineTool where
  parseJSON :: Value -> Parser CommandLineTool
  parseJSON (Object obj) = do
    (clt_id :: String)            <- obj .: "id"
    (clt_inputs :: [CLInput])     <- obj .: "inputs"
    (clt_outputs :: [CLOutput])   <- obj .: "outputs"
    (clt_baseCmd :: BaseCmd )     <- obj .: "baseCommand"
    (clt_stdout :: Maybe String)  <- obj .:? "stdout"
    (clt_args :: Maybe [Arg])     <- obj .:? "arguments"
    (clt_reqs :: Maybe Reqs)      <- obj .:? "requirements"
    ("CommandLineTool" :: String) <- obj .: "class"
    return $ CLTool
      { cl_inputs  = clt_inputs
      , cl_outputs = clt_outputs
      , cl_class   = "CommandLineTool"
      , cl_baseCmd = toStrList clt_baseCmd
      , cl_stdout  = clt_stdout
      , cl_args    = clt_args
      , cl_id      = clt_id
      , cl_reqs    = clt_reqs
      }

    where
      toStrList :: BaseCmd -> [String]
      toStrList (BaseCmd (Left s)) = [s]
      toStrList (BaseCmd (Right xs)) = xs

  parseJSON _ = fail "Tools should be JSON objects."


newtype BaseCmd = BaseCmd (Either String [String])

instance FromJSON BaseCmd where
  parseJSON :: Value -> Parser BaseCmd
  parseJSON (String text) = return $
    BaseCmd $ Left $ T.unpack text
  parseJSON (Array arr) = do
    let getStr = parseJSON :: Value -> Parser String
    xs <- mapM getStr arr
    return $ BaseCmd $ Right $ V.toList xs
  parseJSON _ = fail "Could not parse baseCmd of inputs."


-- *** CLInput parsing
--------------------------------------------------------------------------------

instance FromJSON CLInput where
  parseJSON :: Value -> Parser CLInput
  parseJSON (Object obj) = do
    (clinp_id :: String) <- obj .: "id"
    (clinp_ty :: CWLType) <- obj .: "type"
    (clinp_bind :: Maybe CommandLineBind) <- obj .:? "inputBinding"
    return $ CLInput
      { cl_input_id = clinp_id
      , cl_input_type = clinp_ty
      , cl_input_bind = clinp_bind
      }
  parseJSON _ = fail "cltool-input is a json object"

  parseJSONList :: Value -> Parser [CLInput]
  parseJSONList (Object obj) = do
    let assocList = map (onFst T.unpack) $ M.toList obj
    let getTyandBind = parseJSON :: Value -> Parser NoIdCLIn
    let assocIdInputParse = mapM (onSndM getTyandBind) assocList
    assocIdInpList <- assocIdInputParse
    return $ map clInpPair assocIdInpList where
      clInpPair :: (String, NoIdCLIn) -> CLInput
      clInpPair (nm, NoIdIn ty bind) = CLInput nm ty bind
  parseJSONList (Array arr) =
    let oneParse = (parseJSON :: Value -> Parser CLInput) in
    fmap V.toList $ V.mapM oneParse arr
  parseJSONList _ = fail "Parse error: invalid input format."


data NoIdCLIn where
  NoIdIn ::
    { nmInTy :: CWLType
    , nmInBind :: Maybe CommandLineBind
    } -> NoIdCLIn

instance FromJSON NoIdCLIn where
  parseJSON (Object obj) = do
    (ty :: CWLType) <- obj .: "type"
    (bind :: Maybe CommandLineBind) <- obj .:? "inputBinding"
    return $ NoIdIn ty bind
  parseJSON s@(String _) = do
    (ty :: CWLType) <- parseJSON s
    return $ NoIdIn ty Nothing
  parseJSON _ = fail "Parse error in input map."





-- *** CLOutput parsing
--------------------------------------------------------------------------------

instance FromJSON CLOutput where
  parseJSON :: Value -> Parser CLOutput
  parseJSON (Object obj) = do
    (clout_id :: String) <- obj .: "id"
    (clout_ty :: CWLType) <- obj .: "type"
    (clout_outbind :: CLOutBind) <- obj .: "outputBinding"
    return $ CLOutput
      { cl_output_id = clout_id
      , cl_output_type = clout_ty
      , cl_output_bind = clout_outbind
      }
  parseJSON _ = fail "cltool-output is a json object"

  parseJSONList :: Value -> Parser [CLOutput]
  parseJSONList (Object obj) = do
    let assocList = map (onFst T.unpack) $ M.toList obj
    let getNoIdessOutput =
          parseJSON :: Value -> Parser NoIdCLOut
    let assocListCLOut = mapM (onSndM getNoIdessOutput) assocList
    clOutPairs <- assocListCLOut
    return $ map clOutPair clOutPairs
    where
      clOutPair :: (String, NoIdCLOut) -> CLOutput
      clOutPair (id',NoIdOut ty bind) = CLOutput id' ty bind
  parseJSONList (Array arr) = 
    let oneParse = (parseJSON :: Value -> Parser CLOutput) in
    fmap V.toList $ V.mapM oneParse arr
  parseJSONList _ = fail "Parse error on outputs."


data NoIdCLOut where
  NoIdOut ::
    { nmOutTy :: CWLType
    , nmOutBind :: CLOutBind
    } -> NoIdCLOut

instance FromJSON NoIdCLOut where
  parseJSON :: Value -> Parser NoIdCLOut
  parseJSON (Object obj) = do
    (nlcloutTy :: CWLType) <- obj .: "type"
    (nlcloutBind :: CLOutBind) <- obj .: "outputBinding"
    return $ NoIdOut nlcloutTy nlcloutBind
  parseJSON _ = fail "Could not parse outputs."



-- *** Parsing Binders
--------------------------------------------------------------------------------

instance FromJSON CLOutBind where
  parseJSON :: Value -> Parser CLOutBind
  parseJSON (Object obj) = do
    (glob :: String) <- obj .: "glob"
    return $ CLOutBind glob
  parseJSON _ = fail "cltool outbind is a json object"


instance FromJSON CommandLineBind where
  parseJSON :: Value -> Parser CommandLineBind
  parseJSON (Object obj) = do
    (pos :: Int) <- obj .: "position"
    (pref :: Maybe String) <- obj .:? "prefix"
    return $ CLBind
      { clbind_position = pos
      , clbind_prefix = pref
      }
  parseJSON _ = fail "inputBinding is a json object"


instance FromJSON Arg where
  parseJSON :: Value -> Parser Arg
  parseJSON (Object obj) = do
    (pos :: Int) <- obj .: "position"
    (pref :: Maybe String) <- obj .:? "prefix"
    (str :: String) <- obj .: "valueFrom"
    return $ Arg str pref pos
  parseJSON x = fail ("Could not parse argument: " ++ show x)

  parseJSONList :: Value -> Parser [Arg]
  parseJSONList (Array arr) = do
    case mapM toStr arr of
      Just strVector -> do
        -- Example of negNumbering:
        -- [(0,x),(1,y),(2,z)] -> [(-3,x),(-2,y),(-1,z)]
        let negNumbering = negIndex . I.indexed
        let posList = negNumbering $ V.toList strVector
        return $ map toArg posList
      Nothing ->
        let parseOne = (parseJSON :: Value -> Parser Arg) in
        fmap V.toList $ traverse parseOne arr
    where
      toStr :: Value -> Maybe String
      toStr (String text) = return $ T.unpack text
      toStr _ = Nothing

      negIndex :: [(Int,a)] -> [(Int,a)]
      negIndex xs = map (onFst subIndex) xs where
        subIndex i = i - (length xs + 1)

      toArg :: (Int, String) -> Arg
      toArg (pos, s) = Arg s Nothing pos

  parseJSONList x = fail $
    "Could not parse list of arguments: " ++  show x


-- ** Workflow
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


instance FromJSON Workflow where
  parseJSON :: Value -> Parser Workflow
  parseJSON (Object obj) = do
    (inp :: [InputParam]) <- obj .: "inputs"
    (out :: [WflowOutput]) <- obj .: "outputs"
    ("Workflow" :: String) <- obj .: "class"
    (wfSteps :: [WflowStep]) <- obj .: "steps"
    return $ Workflow
      { w_inputs = inp
      , w_outputs = out
      , w_class = "Workflow"
      , w_steps = wfSteps
      }
  parseJSON _ = fail "Workflows are objects"



-- *** InputParam
--------------------------------------------------------------------------------

-- Note: Only one format for InputParam considered: a list.
instance FromJSON InputParam where
  parseJSON :: Value -> Parser InputParam
  parseJSON (Object obj) = do
    (ip_id :: String) <- obj .: "id"
    (ip_type :: CWLType) <- obj .: "type"
    return $ InputParam
      { inp_id = ip_id
      , inp_type = ip_type
      }
  parseJSON _ = fail "InputParams are objects"

  parseJSONList :: Value -> Parser [InputParam]
  parseJSONList (Object obj) = do
    let assocList = map (onFst T.unpack) $ M.toList obj
    let getType = parseJSON :: Value -> Parser CWLType
    let assocList' = mapM (onSndM getType) assocList
    idTypePairs <- assocList'
    return $ map (\(iden,ty) -> InputParam iden ty) idTypePairs
  parseJSONList (Array arr) =
    let oneParse = (parseJSON :: Value -> Parser InputParam) in
    fmap V.toList $ V.mapM oneParse arr
  parseJSONList _ = fail "Parse error on workflow inputs."



-- *** Workflow Outputs
--------------------------------------------------------------------------------

instance FromJSON WflowOutput where
  parseJSON :: Value -> Parser WflowOutput
  parseJSON (Object obj) = do
    (w_id :: String) <- obj .: "id"
    (w_type :: CWLType) <- obj .: "type"
    (OutputSrc source :: OutputSrc) <- obj .: "outputSource"
    return $ WflowOutput
      { wfout_id = w_id
      , wfout_type = w_type
      , wfout_source = source
      }
  parseJSON _ = fail "WflowOutputs are objects"

  parseJSONList :: Value -> Parser [WflowOutput]
  parseJSONList (Object obj) = do
    let assocList = map (onFst T.unpack) $ M.toList obj
    let getNoIdWfOut = parseJSON :: Value -> Parser NoIdWfOut
    let assocList' = mapM (onSndM getNoIdWfOut) assocList
    pairs <- assocList'
    return $ map mkWflowOutput pairs where
      mkWflowOutput (iden, NoIdWfOut ty src) = WflowOutput iden ty src
  parseJSONList (Array arr) =
    let oneParse = (parseJSON :: Value -> Parser WflowOutput) in
    fmap V.toList $ V.mapM oneParse arr
  parseJSONList _ = fail "Parse error on workflow outputs."




data NoIdWfOut where
  NoIdWfOut ::
    { noIdwfoTy :: CWLType
    , noIdwfoSrc :: String
    } -> NoIdWfOut

instance FromJSON NoIdWfOut where
  parseJSON :: Value -> Parser NoIdWfOut
  parseJSON (Object obj) = do
    (w_type :: CWLType) <- obj .: "type"
    (OutputSrc source :: OutputSrc) <- obj .: "outputSource"
    return $ NoIdWfOut w_type source
  parseJSON x = fail $
    "Could not parse the workflow output: \n" ++ show x



newtype OutputSrc = OutputSrc String

instance FromJSON OutputSrc where
  parseJSON (String text) = return $ OutputSrc $ T.unpack text
  parseJSON x@(Array arr) = do
    let maybeArrHead = arr V.!? 0
    let maybeOutSrc = fmap parseJSON maybeArrHead
    case maybeOutSrc of
      Nothing ->
        fail $ "Could not parse 'outputSource': \n" ++ show x
      Just parserOutSrc -> parserOutSrc
  parseJSON x = fail $ "Could not parse 'outputSource': \n" ++ show x




-- *** Workflow Steps
--------------------------------------------------------------------------------


instance FromJSON WflowStep where
  parseJSON :: Value -> Parser WflowStep
  parseJSON (Object obj) = do
    (step_id :: String) <- obj .: "id"
    (step_in :: [WflowStepInput]) <- obj .: "in"
    (step_out :: [WflowStepOutput]) <- obj .: "out"
    (step_run :: String) <- obj .: "run"
    (step_scat :: Maybe StrList)  <- obj .:? "scatter"
    (step_smeth :: Maybe ScatterMethod)  <- obj .:? "scatterMethod"
    let toStrList (StrList l) = l
    let scatL = fmap toStrList step_scat
    return $ WflowStep
      { wfstep_id = step_id
      , wfstep_input = step_in
      , wfstep_output = step_out
      , wfstep_run = step_run
      , wfstep_scat = Scatter <$> scatL <*> step_smeth
      }
  parseJSON _ = fail "WFsteps are objects."

-- TODO: Remove this repeated code. Generalize it.
  parseJSONList :: Value -> Parser [WflowStep]
  parseJSONList (Object obj) = do
    let assocList = map (onFst T.unpack) $ M.toList obj
    let getNoIdStep = parseJSON :: Value -> Parser NoIdStep
    let assocList' = mapM (onSndM getNoIdStep) assocList
    pairs <- assocList'
    return $ map mkWfStep pairs where
      mkWfStep (iden, NoIdStep ins outs run scat) =
        WflowStep iden ins outs run scat
  parseJSONList (Array arr) =
    let oneParse = (parseJSON :: Value -> Parser WflowStep) in
    fmap V.toList $ V.mapM oneParse arr
  parseJSONList x = fail $
    "Parse error on workflow steps: \n" ++ show x


data NoIdStep where
  NoIdStep ::
    { noidstep_in :: [WflowStepInput]
    , noidstep_out :: [WflowStepOutput]
    , noidstep_run :: String
    , noidstep_scat :: Maybe Scatter
    } -> NoIdStep


instance FromJSON NoIdStep where
  parseJSON :: Value -> Parser NoIdStep
  parseJSON (Object obj) = do
    (step_in :: [WflowStepInput]) <- obj .: "in"
    (step_out :: [WflowStepOutput]) <- obj .: "out"
    (step_run :: String) <- obj .: "run"
    (step_scat :: Maybe StrList)  <- obj .:? "scatter"
    (step_smeth :: Maybe ScatterMethod)  <- obj .:? "scatterMethod"
    let toStrList (StrList l) = l
    let scatL = fmap toStrList step_scat
    let scatter = Scatter <$> scatL <*> step_smeth
    return $ NoIdStep step_in step_out step_run scatter
  parseJSON x = fail $
    "Could not parse workflow step: \n" ++ show x


newtype StrList = StrList [String]

instance FromJSON StrList where
  parseJSON :: Value -> Parser StrList
  parseJSON (String text) = return $ StrList [T.unpack text]
  parseJSON x = do
    (x' :: [String]) <- parseJSON x
    return $ StrList x'

instance FromJSON ScatterMethod where
  parseJSON :: Value -> Parser ScatterMethod
  parseJSON (String text) = do
    let str = T.unpack text
    case text of
      "dotproduct" -> return DotScat
      "flat_crossproduct" -> return CrossScat
      _ -> fail $ "Scatter method not supported: " ++ str
  parseJSON x =
    fail $ "Invalid input for 'scatterField': " ++ show x



instance FromJSON WflowStepInput where
  parseJSON :: Value -> Parser WflowStepInput
  parseJSON (Object obj) = do
    (step_id :: String) <- obj .: "id"
    (step_source :: String) <- obj .: "source"
    return $ WflowStepInput
      { wfstep_input_id = step_id
      , wfstep_input_source = step_source
      }
  parseJSON _ = fail "WFsteps Inputs are objects."

  parseJSONList :: Value -> Parser [WflowStepInput]
  parseJSONList (Object obj) = do
    let assocList = map (onFst T.unpack) $ M.toList obj
    let getSource = parseJSON :: Value -> Parser String
    let assocList' = mapM (onSndM getSource) assocList
    pairs <- assocList'
    return $ map mkWfStepIn pairs where
      mkWfStepIn (iden, source) = WflowStepInput iden source
  parseJSONList (Array arr) =
    let oneParse = (parseJSON :: Value -> Parser WflowStepInput) in
    fmap V.toList $ V.mapM oneParse arr
  parseJSONList x = fail $
    "Parse error on workflow step inputs: \n" ++ show x



instance FromJSON WflowStepOutput where
  parseJSON :: Value -> Parser WflowStepOutput
  parseJSON (String text) = return $ WflowStepOutput $ T.unpack text
  parseJSON (Object obj) = do
    (step_id :: String) <- obj .: "id"
    return $ WflowStepOutput step_id
  parseJSON _ = fail "WFsteps Outputs are objects."




-- ** Requirements
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


instance FromJSON Reqs where
  parseJSON :: Value -> Parser Reqs
  parseJSON (Array arr) = do
    let docker = getDockerReq arr
    let env = getEnvReq arr
    return $ Reqs env docker
  parseJSON x = fail $
    "Could not parse requirements: \n" ++ show x

instance FromJSON EnvReq where
  parseJSON :: Value -> Parser EnvReq
  parseJSON (Object obj) = do
    ("EnvVarRequirement" :: String) <- obj .: "class"
    (envDef :: M.HashMap T.Text T.Text) <- obj .: "envDef"
    return $ EnvReq envDef
  parseJSON _ = fail ""

instance FromJSON DockerReq where
  parseJSON :: Value -> Parser DockerReq
  parseJSON (Object obj) = do
    ("DockerRequirement" :: String) <- obj .: "class"
    (image :: String) <- obj .: "dockerPull"
    return $ DockerReq image
  parseJSON _ = fail ""


-- Note: type Array = V.Vector Value

getReq :: FromJSON a => Array -> Maybe a
getReq = firstJust $ parseMaybe parseJSON

getEnvReq :: Array -> Maybe EnvReq
getEnvReq = getReq

getDockerReq :: Array -> Maybe DockerReq
getDockerReq = getReq

firstJust :: (a -> Maybe b) -> V.Vector a -> Maybe b
firstJust f = maybeHead . V.mapMaybe f

maybeHead :: V.Vector a -> Maybe a
maybeHead xs = xs V.!? 0


-- * Library
--------------------------------------------------------------------------------

onSndM :: Monad m => (a -> m b) -> (x,a) -> m (x, b)
onSndM f (x,a) = do
  b <- f a
  return (x,b)

onFst :: (a -> b) -> (a,y) -> (b, y)
onFst f (a,y) = (f a, y)



