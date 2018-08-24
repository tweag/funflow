{-# LANGUAGE StrictData           #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE Arrows               #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE DeriveAnyClass     #-}

{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : FlowCmdWrapper
Description : Convert a 'CommandLineTool' to a 'FlowCmdWrapper'

This module exports functions for processing a 'CommandLineTool'
into an intermediate type, 'FlowCmdWrapper' which holds all the pieces needed
to represent that CWL command line tool as an external step.

-}




module Control.Funflow.CWL.Convert.FlowCmdWrapper
  ( FlowCmdWrapper (..)
  , mkFlowCmdWrapper
  )
  where


-- Funflow
import Control.Arrow
import Control.Funflow
import qualified Control.Funflow.ContentStore as CS
import Control.Funflow.ContentStore ( Content (..) )

-- Not funflow
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.Scientific as Sc
import Data.Typeable
import Data.List ( sortBy, nub )
import Path ( Path, Rel, File, parseRelFile, Dir )
import Data.Kind
import Control.Monad.Catch ( throwM, Exception, catch )
import Data.Maybe ( mapMaybe )
import Data.Functor.Identity
import Control.Monad ()

-- Internal
import Data.Existentials
import Data.HList
import Data.Vec
import Data.ErrorMonad
import Control.Funflow.CWL.Types
import Control.Funflow.CWL.Util.StringParse (parseGlobStr)



-- # Top Data Definitions
--------------------------------------------------------------------------------

-- $flowCmdWrapper
data FlowCmdWrapper where
  FlowCmdWrapper :: (TraversibleL l, TraversibleL l') =>
    { fInSchema  :: Schema l
    , fOutSchema :: Schema l'
    , cmd :: String
    , paramMaker :: HList l -> [Param]
    , extractItemOutputs :: (HList l, CS.Item) ==> HList l'
    , getEnvVar :: HList l -> [(T.Text, T.Text)]
    , dock :: Maybe DockerReq
    , stdout :: Maybe (Path Rel File)
    } -> FlowCmdWrapper

-- $flowCmdWrapper
--
-- This is a data structure that holds all the puzzle pieces needed to wrap one
-- of funflow's external steps as a flow between two generic records. One
-- puzzle piece is a specification of the input and outputs of these records:
-- i.e., the field names and types of each record. We call this specification a
-- "Schema". See the Schema module for more information.
--
-- Besides schemas, what else do we need to wrap a flow between two row types
-- from an external step?  Well, recall that an external step is basically a
-- flow that executes a command line argument and produces an "Item" in the
-- content store. To wrap this thing, we need a way to get out the command line
-- parameters from the input HList: 'paramMaker'. To create the output HList we
-- have 'extractItemOutputs' which, as the name suggests extracts the desired
-- outputs from the "Item" using the input list to resolve output names which
-- depend on inputs (i.e., parameter references).
--
-- Lastly, we support other configuration of external steps like requiring
-- docker containers or environment variables or capturing standard output.


-- * Making the FlowCmdWrapper
--------------------------------------------------------------------------------

mkFlowCmdWrapper :: CommandLineTool -> ErrM FlowCmdWrapper
mkFlowCmdWrapper tool@(CLTool {cl_baseCmd = (cmd':strArgs)}) = do
  let processInput = processCLInputs givenArgs ins
  let processInput' = catch processInput (badPositions tool)
  ProcCLIns inSchema' paramMaker' <- processInput'
  let paramMaker'' = addStrArgs strArgs paramMaker'
  getEnvVar' <- mkEnvVarFn envList inSchema'
  stdout' <- traverse parseRelFile $ cl_stdout tool
  let processOutput = processCLOutputs inSchema' outs
  ProcCLOuts outSchema' extractOuts <- processOutput
  return $ FlowCmdWrapper
    { fInSchema = inSchema'
    , fOutSchema = outSchema'
    , cmd = cmd'
    , paramMaker = paramMaker''
    , stdout = stdout'
    , extractItemOutputs = extractOuts
    , dock = theDocker
    , getEnvVar = getEnvVar'
    }
  where
    ins = cl_inputs tool
    outs = cl_outputs tool
    givenArgs = maybeList $ cl_args tool
    reqs = cl_reqs tool
    theDocker = getDocker reqs
    envList = getEnvList reqs


    addStrArgs ::
      [String] -> (HList l -> [Param]) -> HList l -> [Param]
    addStrArgs xs paramMaker''' hlist =
      map stringParam xs ++ paramMaker''' hlist

    maybeList :: Maybe [a] -> [a]
    maybeList Nothing   = []
    maybeList (Just xs) = xs

    getDocker :: Maybe Reqs -> Maybe DockerReq
    getDocker r = do
      Reqs _ dockerReq <- r
      dockerReq

    getEnvList :: Maybe Reqs -> [(T.Text, T.Text)]
    getEnvList Nothing = []
    getEnvList (Just reqs') = getEL reqs' where
      getEL (Reqs Nothing _) = []
      getEL (Reqs (Just (EnvReq envs)) _) = M.toList envs

    badPositions :: CommandLineTool -> NonUniquePositions -> ErrM a
    badPositions tool' (NonUniquePositions pos) =
      throwM $ ErrMsg $ "Non unique positions for\
        \inputs on command line: " ++ show pos ++
        " in tool: \n" ++ show tool'

mkFlowCmdWrapper tool =
  throwM $ ErrMsg $ "This tool has no base command: \n" ++ show tool



-- * Processing [CLInput]
--------------------------------------------------------------------------------

-- 'ProcCLIns' represents processed 'CLInput's; we determine the input schema
-- and the 'paramMaker' and wrap these in an existential type.
data ProcCLIns where
  ProcCLIns :: (TraversibleL l) =>
    { procCLInSch :: Schema l
    , procCLparamMaker :: PMaker l
    } -> ProcCLIns

-- | The type of the 'paramMaker'.
type PMaker l = HList l -> [Param]
-- | A  'paramMaker' that produces 'Param's that are not in the right order on
-- the command line. These 'Param's are paired with a 'SortKey' that defines
-- in what order they should appear on the command line.
type UnsortPMaker l = HList l -> [(Param, SortKey)]
-- | The unique position of the parameter in the command line:
-- @cmd pos1 pos2 pos3 ...@.
type SortKey = Int

-- | The same as 'ProcCLIns' but with an unsorted 'paramMaker'.
data UnsortIns l where
  UnsortIns :: TraversibleL l =>
    { unsortInSch :: Schema l
    , unsortPMaker :: UnsortPMaker l
    } -> UnsortIns l


-- | An error type that we raise whenever a tool does not specify unique
-- positions for all the parameters which appear on the command line.
newtype NonUniquePositions =
  NonUniquePositions [Int]

deriving instance Exception Int
deriving instance Show NonUniquePositions
deriving instance Exception NonUniquePositions


-- | CWL tools take a list of inputs but also have fixed arguments which appear
-- on the command line. 'processCLInputs' takes those fixed arguments and the
-- 'CLInput's and processes them.
processCLInputs :: [Arg] -> [CLInput] -> ErrM ProcCLIns
processCLInputs args ins = do
  let argPos = map arg_pos args
  let insPos = mapMaybe (cl_input_bind >=>> clbind_position) ins
  let positions = argPos ++ insPos
  let len = length positions
  let err = NonUniquePositions positions
  guardErrM (length (nub positions) == len) err

  let recur = return $ unsortedProcIns ins
  Identity (Some (UnsortIns schema paramMaker')) <- recur
  let argParams = map argToParam args
  return $ ProcCLIns schema (sort paramMaker' argParams)

  where
    -- | This sorts an unsorted 'paramMaker' and inserts the argument
    -- parameters in the process.
    sort :: UnsortPMaker l -> [(Param, SortKey)] -> PMaker l
    sort unsortedPMaker argParams hl =
      let comp (_,k) (_,k') = compare k k' in
      -- Note: sortBy is stable.
      map fst . sortBy comp $ unsortedPMaker hl ++ argParams

    argToParam :: Arg -> (Param, SortKey)
    argToParam (Arg str Nothing pos) = (stringParam str, pos)
    argToParam (Arg str (Just p) pos) = case stringParam str of
      Param paramPaths ->
        let
          prefix = ParamText $ T.pack $ p ++ " "
          paramPaths' =  prefix:paramPaths
        in
          (Param paramPaths', pos)

    (>=>>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
    fm >=>> g = \a -> fmap g $ fm a



-- | This processes the 'CLInput's to produce the inner unsorted process type:
-- 'UnsortIns'. It uses the Identity monad to avoid long lines of irrefutable
-- pattern matches by doing @x <- return irrefutable@.
unsortedProcIns :: [CLInput] -> Identity (Some UnsortIns)
unsortedProcIns [] =
  return $ Some $ UnsortIns (Schema proxyListNil VNil) (const [])
unsortedProcIns (c:cs) = do
  let recur = unsortedProcIns cs
  let pkit = getParamKit $ cl_input_type c
  (Some (UnsortIns schema csPMaker)) <- recur
  (ParamKit tyOfc cToParams) <- return pkit
  let newTys = proxyListCons tyOfc (sc_types schema)
  let newIds = (cl_input_id c) :.> (sc_ids schema)
  let newSch = Schema newTys newIds
  case (cl_input_bind c) of
    Nothing -> do
      let newPMaker = csPMaker . hTail
      return $ Some $ UnsortIns newSch newPMaker
    Just bind -> do
      let pref = clbind_prefix bind
      let cToParam' = cToParams pref
      let key = clbind_position bind
      let pMaker = addParam tyOfc key cToParam' csPMaker
      return $ Some $ UnsortIns newSch pMaker

  where
    addParam ::
      TyProxy t -> SortKey -> (t -> [Param]) ->
      (HList l -> [(Param, SortKey)]) ->
      HList (t:l) -> [(Param, SortKey)]
    addParam _ key toParams paramMaker' hlist =
      (zip params keys) ++ (paramMaker' $ hTail hlist) where
        params = toParams $ hHead hlist
        lenParams = length params
        keys = replicate lenParams key


proxyListNil :: ProxyList '[]
proxyListNil = ProxyList


-- ** ParamKit
--------------------------------------------------------------------------------

-- This is not an abstraction, it just lessens line length:
type MaybePref = Maybe String

-- | A 'ParamKit' is a kit to take some type @t@ along with a possible prefix,
-- which is a 'Maybe String' and produce a list of parameters.
data ParamKit where
  ParamKit ::(Typeable t, Show t) =>
      { pkitTy :: TyProxy t
      , pkitToParams :: MaybePref -> t -> [Param]
      } -> ParamKit

-- | This interprets out AST for a 'CWLType' as a Haskell type and provides a
-- kit for taking something of that type and making a list of parameters.
getParamKit :: CWLType -> ParamKit
getParamKit CWLStr  = strParamKit
getParamKit CWLFile = cfParamKit
getParamKit CWLBool = boolParamKit
getParamKit CWLNum  = numParamKit
getParamKit CWLDir  = dirParamKit
getParamKit (CWLMaybe t) =  case getParamKit t of
  ParamKit ty toParams' -> ParamKit newTy newtoParams
    where
      newTy = addMaybeProxy ty
      newtoParams = maybeToParam toParams'

      addMaybeProxy :: (Typeable t, Show t) =>
        TyProxy t -> TyProxy (Maybe t)
      addMaybeProxy Proxy = Proxy

      maybeToParam :: (Typeable t, Show t) =>
        (MaybePref -> t -> [Param]) ->
        MaybePref -> Maybe t -> [Param]
      maybeToParam _ _ Nothing = [Param []]
      maybeToParam f p (Just x) = f p x

getParamKit (CWLArr t) = case getParamKit t of
  ParamKit ty toParams' -> ParamKit newTy newToParam
    where
    simpleToParams = toParams' Nothing
    newToParam = addPref $ listToParams simpleToParams
    newTy = listType ty

    -- | Extend the one-t converter to a list-t,
    -- converter accounting for spacing
    listToParams :: (t -> [Param]) -> [t] -> [Param]
    listToParams = concatMap

    listType :: TyProxy t -> TyProxy [t]
    listType _ = Proxy


-- | This takes a simple function to make a list of parameters and adds a
-- prefix in the canonical way.
addPref :: (t -> [Param]) -> MaybePref -> t -> [Param]
addPref directParam Nothing t = directParam t
addPref directParam (Just p) t = stringParam p : (directParam t)


injList :: (a -> b) -> a -> [b]
injList f a = [f a]


-- *** Fixed Param Kits

strParamKit :: ParamKit
strParamKit = ParamKit strProxy (addPref $ injList stringParam)

cfParamKit :: ParamKit
cfParamKit = ParamKit cfProxy (addPref $ injList contentParam)

boolParamKit :: ParamKit
boolParamKit = ParamKit boolProxy boolToParam
  where
    boolToParam :: MaybePref -> Bool -> [Param]
    boolToParam _ False = [Param []]
    boolToParam p True = addPref (const $ [Param []]) p ()

numParamKit :: ParamKit
numParamKit = ParamKit numProxy (addPref $ injList numParam)
  where
    numParam :: Sc.Scientific -> Param
    numParam n = stringParam $ show n

dirParamKit :: ParamKit
dirParamKit = ParamKit dirProxy (addPref $ injList contentParam)




-- *** Fixed Proxies

cfProxy :: TyProxy (Content File)
cfProxy = Proxy

cfsProxy :: TyProxy [Content File]
cfsProxy = Proxy

dirProxy :: TyProxy (Content Dir)
dirProxy = Proxy

strProxy :: TyProxy String
strProxy = Proxy

boolProxy :: TyProxy Bool
boolProxy = Proxy

numProxy :: TyProxy Sc.Scientific
numProxy = Proxy


-- ** Getting out Environment variable bindings
--------------------------------------------------------------------------------

-- | Since some environment variable bindings depend upon the inputs, this
-- takes our environment bindings, a specification of the inputs via the schema
-- and creates a function that takes the input record and produces the exact
-- environment bindings we want.
mkEnvVarFn :: TraversibleL l =>
  [(T.Text, T.Text)] ->
  Schema l ->
  ErrM (HList l -> [(T.Text, T.Text)])
mkEnvVarFn [] _ = return $ injectA [] where
  injectA x = proc _ -> do (returnA -< x)
mkEnvVarFn ((var,val):xs) sch@(Schema tys ids) = do
  someEnvVars <- mkEnvVarFn xs sch
  let valStr = T.unpack val
  case parseGlobStr valStr of
    Nothing -> return $ attachLit (var,val) someEnvVars
    Just globStr -> do
      let field = (strProxy, globStr)
      let sch' = Schema tys ids
      let indexSch = indexSchema sch' field
      let err = ErrMsg $ "Param reference in EnvVar\
        \Requirement bad: " ++ globStr
      schIndex <- injectMaybe indexSch err
      return $ attachRef schIndex var someEnvVars

  where
    attachLit :: TraversibleL l =>
      (T.Text, T.Text) -> GetEnvVar l -> GetEnvVar l
    attachLit (nm, val') getEnvs hl =
      (nm, val'): (getEnvs hl)

    attachRef :: TraversibleL l =>
      SchemaIndex l String -> T.Text -> GetEnvVar l -> GetEnvVar l
    attachRef (SchemaIndex Refl i) nm getEnvs hlist =
      let valParam = T.pack $ hIndex hlist i in
      (nm,valParam): (getEnvs hlist)


type GetEnvVar l = HList l -> [(T.Text, T.Text)]




-- * Processing [CLOutput]
--------------------------------------------------------------------------------

-- | When we process 'CLOutput's we want to determine the output schema and the
-- way in which we will take the result of executing a command, a 'CS.Item' and
-- produce the output heterogeneous list. That is, we want to make the
-- 'extractItemOutputs' function.
data ProcCLOuts :: [Type] -> Type where
  ProcCLOuts :: TraversibleL l' =>
    { procOutSchema  :: Schema l'
    , procOutextOuts :: (HList l, CS.Item) ==> HList l'
    } -> ProcCLOuts l



processCLOutputs :: TraversibleL l =>
  Schema l -> [CLOutput] -> ErrM (ProcCLOuts l)
processCLOutputs _ [] = return $
  ProcCLOuts (Schema proxyListNil VNil) (step $ const HNil)
processCLOutputs inSch (o:os) = do
  let recur = processCLOutputs inSch os
  ProcCLOuts (Schema outTys outIds') extractOuts <- recur
  let oId = cl_output_id o
  let oTp = cl_output_type o
  let bind = cl_output_bind o
  let outputKit = getOutputKit inSch oTp bind
  OutKit outTy extractOneOut <- outputKit
  let newProxyList = proxyListCons outTy outTys
  let newIds = oId :.> outIds'
  let newSchema = Schema newProxyList newIds
  let newExtOuts = addOneOut extractOuts extractOneOut
  return $ ProcCLOuts newSchema newExtOuts

  where
    addOneOut :: TraversibleL l =>
      ((HList l, CS.Item) ==> HList l') ->
      ((HList l, CS.Item) ==> t)        ->
      ((HList l, CS.Item) ==> HList (t:l'))
    addOneOut extOuts extOut = proc stepInfo -> do
      tailOuts <- extOuts -< stepInfo
      headOut <-  extOut  -< stepInfo
      returnA -< (headOut :> tailOuts)


-- ** OutputKit
--------------------------------------------------------------------------------


-- | This is a kit to extract one output of type @t@ from the 'CS.Item'. It
-- also uses the input record (@HList l@) in case, say, the output file names
-- depend on the input.
data OutputKit :: [Type] -> Type where
  OutKit :: (Typeable t, Show t, TraversibleL l) =>
    TyProxy t -> ((HList l , CS.Item) ==> t) -> OutputKit l

-- | Recall that 'CLOutBind' is a type to represent instructions for extracting
-- output from a directory after a command has been run. This function takes
-- the specification of the input record, the type we expect to get from the
-- 'CS.Item' and these instructions in 'CLOutBind' to produce a kit -- a Flow
-- for extracting that one output.
getOutputKit :: TraversibleL l =>
  Schema l -> CWLType -> CLOutBind -> ErrM (OutputKit l)
getOutputKit inSchema' CWLFile (CLOutBind glob) = do
  case parseGlobStr glob of
    Nothing  -> return $ OutKit cfProxy (getLitCF glob (sc_types inSchema'))
    Just ref -> do
      let field = (strProxy, ref)
      let maybeIndex = indexSchema inSchema' field
      let err = ErrMsg $
                "The glob input param ref " ++ ref ++ " is invalid."
      schemaIndex <- injectMaybe maybeIndex err
      let extractCF = getRefCF schemaIndex
      return $ OutKit cfProxy extractCF
getOutputKit inSchema' (CWLArr CWLFile) (CLOutBind glob) = do
  case parseGlobStr glob of
    Nothing  -> return $ OutKit cfsProxy (getFilesLit glob)
    Just ref -> do
      let field = (strProxy, ref)
      let maybeIndex = indexSchema inSchema' field
      let err = ErrMsg $
                "The glob input param ref " ++ ref ++ " is invalid."
      schemaIndex <- injectMaybe maybeIndex err
      return $ OutKit cfsProxy $ getFilesRef schemaIndex
getOutputKit _ ty _ = throwM $ ErrMsg $
  "Cannot extract output type: " ++ show ty


-- *** Extracting Content Files
--------------------------------------------------------------------------------

type CF = Content File



-- | This gets a single content file from a glob pattern
getLitCF :: String -> ProxyList l -> (HList l, CS.Item) ==> CF
getLitCF fileName _ = proc stepInfo -> do
  let item = snd stepInfo
  contFile <- globFile -< (item, fileName)
  returnA -< contFile


-- | This extracts a content file given a reference/index to an
-- input parameter which is a glob pattern.
getRefCF :: SchemaIndex l String -> (HList l, CS.Item) ==> CF
getRefCF (SchemaIndex Refl index) = proc stepInfo -> do
  let fileNm = hIndex (fst stepInfo) index
  let item = snd stepInfo
  contFile <- globFile -< (item, fileNm)
  returnA -< contFile


-- | This is analogous to getLitCF.
getFilesLit :: String -> (a, CS.Item) ==> [CF]
getFilesLit globStr = proc globInfo -> do
  let item = snd globInfo
  let itemDir = All item
  globDir -< (itemDir, globStr)


-- | This is analogous to getRefCF.
getFilesRef :: SchemaIndex l String -> (HList l, CS.Item) ==> [CF]
getFilesRef (SchemaIndex Refl index) = proc globInfo -> do
  let globStr = hIndex (fst globInfo) index
  let item = snd globInfo
  let itemDir = All item
  globDir -< (itemDir, globStr)




-- | This extracts one file from the store given a
-- glob pattern. It fails if no files or too many
-- files match the glob pattern.
globFile :: (CS.Item, String) ==> CF
globFile = proc globInfo -> do
  let item = fst globInfo
  let globStr = snd globInfo
  let itemDir = All item
  cfs <- globDir -< (itemDir, globStr)
  case cfs of
    (cf:[]) -> do
      returnA -< cf
    [] -> do
      failFlow -< ErrMsg $
        "The glob string had no matches: " ++ globStr
    _ -> do
      failFlow -< ErrMsg $
        "The glob string had more than one match: " ++ globStr



-- | This is the equivalent of 'error' for Flows.
failFlow :: Exception e => e ==> a
failFlow = stepIO throwM


