{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}


{-|
Module      : Workflow
Description : Convert a 'Workflow' to a 'SomeFlow'

This module exports exports key functions for converting our
internal workflow representation into a 'SomeFlow', a flow between
two records. It also exports a function to convert either a tool or a
workflow into a 'SomeFlow'.

-}


module Control.Funflow.CWL.Convert.Workflow
  ( convertWorkflow
  , convertCWLFile
  ) where

import           Control.Arrow
import           Control.Monad
    ((>=>))
import qualified Data.HashMap.Strict as M

import           Control.Funflow
    (step)
import           Data.Proxy
import           Control.Exception
import           Data.Typeable


import           Control.Funflow.CWL.Convert.Tool
import           Control.Funflow.CWL.Parse
import           Control.Funflow.CWL.Types
import           Control.Funflow.CWL.Util.CWLUtil
import           Control.Funflow.CWL.Util.StringParse
    (parseSourceRef, parseDestRef)
import           Control.Funflow.CWL.Convert.Wiring
import           Control.Funflow.CWL.Convert.Scatter
import           Data.Existentials
import           Data.Graph
    ( getPreOrder, GraphExcept (..) )
import           Data.HList
import           Data.Vec
import           Data.ErrorMonad



-- * Step 1: Workflow -> A Linear Pre-order
--------------------------------------------------------------------------------


-- | These are nodes in the dag of a workflow.
data WfNode where
  WfStep :: WflowStep -> WfNode
  WfIn   :: InputParam -> WfNode
  WfOut  :: WflowOutput -> WfNode
  deriving (Show, Eq)

type WfGraph = (M.HashMap WfId WfNode, GetWfNodeParents)
type WfId = String
type GetWfNodeParents =  WfNode -> ErrM [WfId]


getWfPreOrder :: Workflow -> ErrM [[WfNode]]
getWfPreOrder wf =
  interpretErrors . (mkNodes >=> getPreOrder) $ wf
  where
    interpretErrors :: ErrM [[WfNode]] -> ErrM [[WfNode]]
    interpretErrors = modErrM toWfError

    toWfError :: SomeException -> SomeException
    toWfError (SomeException (e :: e)) =
      case eqT :: Maybe (e :~: GraphExcept String) of
        Just Refl -> case e of
          CyclicGraph    -> SomeException $ ErrMsg $
            "This workflow is cyclic: \n" ++ show wf
          BadNodeRef ref -> SomeException $ ErrMsg $
            "Bad 'source' " ++ ref ++
            " in workflow: \n" ++ show wf
        Nothing -> SomeException e


type RawWfGraph = M.HashMap WfId WfNode

{-
   Note: in the conversion of nodes in the dag, all non input
   or output nodes have their input ids converted to their
   source name and their output ids qualified by the step id.
-}


mkNodes :: Workflow -> ErrM WfGraph
mkNodes wflow = do
  let ins = w_inputs wflow
  let outs = w_outputs wflow
  let steps = w_steps wflow
  addInPts <- mkInNodes ins M.empty
  addOutPts <- mkOutNodes outs addInPts
  addStepPts <- mkStepNodes steps addOutPts
  return (addStepPts, getParents)
  where
    mkInNodes =  mkSomeNodes (inp_id, WfIn)
    mkOutNodes = mkSomeNodes (wfout_id, WfOut)
    mkStepNodes = mkSomeNodes (wfstep_id, WfStep)

    mkSomeNodes ::
      (a -> String, a -> WfNode) -> [a] -> RawWfGraph -> ErrM RawWfGraph
    mkSomeNodes _ [] pointMap = return pointMap
    mkSomeNodes (getId,toNd) (i:is) pointMap = do
      let idNode = getId i
      let err = ErrMsg $
                "The id " ++ idNode ++
                " is not unique in workflow: \n" ++ show wflow
      guardErrM (not $ M.member idNode pointMap) err
      let mapWithFst = M.insert idNode (toNd i) pointMap
      mkSomeNodes (getId,toNd) is mapWithFst

    getParents :: WfNode -> ErrM [String]
    getParents =
      interpretError . mapM parseSourceRef . getParentRefs
      where
        interpretError :: ErrM [String] -> ErrM [String]
        interpretError = modErrM modParseErr

        modParseErr :: SomeException -> SomeException
        modParseErr (SomeException e) = SomeException $ ErrMsg $
          "Could not parse a 'source': \n" ++ displayException e

        getParentRefs :: WfNode -> [String]
        getParentRefs (WfIn _) = []
        getParentRefs (WfOut wfout) = [wfout_source wfout]
        getParentRefs (WfStep wfstep) =
          map wfstep_input_source $ wfstep_input wfstep


removeInputNodes :: [[WfNode]] -> [[WfNode]]
removeInputNodes = filter nonEmpty . (fmap $ filter isNotInNode) where

  isNotInNode (WfIn _) = False
  isNotInNode _ = True

  nonEmpty [] = False
  nonEmpty _ = True



-- * Step 2: Linear preorder of WfNodes -> Linear preorder of SomeFlows
--------------------------------------------------------------------------------

convertToFlows :: [[WfNode]] -> IOErrM [[SomeFlow]]
convertToFlows = traverse . traverse $ convertNode


-- Note: replace the inputs
-- with the names given as a 'source' and
-- and qualify the outputs with the wfstep's id.
convertNode :: WfNode -> IOErrM SomeFlow
convertNode node = case node of
  WfIn inparam ->
    let
      inputProxy = cwlTyToProxy . inp_type $ inparam
      inputId = inp_id inparam
     in return $ someFlowOne inputProxy inputId
  WfOut outparam ->
    let
      outputProxy = cwlTyToProxy . wfout_type $ outparam
      outputId = wfout_source outparam -- This must be the source
    in return $ someFlowOne outputProxy outputId
  WfStep step' -> do
    let wfstepNm = wfstep_run step'
    cwlFile <- parseCWLFile wfstepNm
    someFlowReg <- convertCWLFile cwlFile
    --lift $ putStrLn ("Original flow:\n" ++ show someFlowReg)
    someFlow <- case wfstep_scat step' of
      Nothing -> return someFlowReg
      Just scat -> do
        --lift $ putStrLn "\n\nScattering as expected"
        --lift $ putStrLn ("Scat Value:" ++ show scat)
        injectErrM $ scatter someFlowReg scat
    --lift $ putStrLn ("Scattered flow:\n" ++ show someFlow)
    let renameIns = renameInputs wfstepNm (wfstep_input step')
    flowRenamed <- injectErrM $ renameIns someFlow
    let qualifyOuts = qualifyOutputs (wfstep_id step')
    return $ qualifyOuts flowRenamed

  where
    flowOne :: Proxy a -> HList (a:'[]) ==> HList (a:'[])
    flowOne _ = returnA

    someFlowOne :: SomeTyProxy -> String -> SomeFlow
    someFlowOne (SomeTyProxy proxy) identifier =
      SomeFlow
        { theFlow = flowOne proxy
        , inSchema = Schema ProxyList (identifier :.> VNil)
        , outSchema = Schema ProxyList (identifier :.> VNil)
        }

    -- | Replace the input id with the source.
    renameInputs :: String -> [WflowStepInput] -> SomeFlow -> ErrM SomeFlow
    renameInputs stepName wfins (SomeFlow flow inSch outSch) = do
      inIds' <- errMInIds
      let inSch' = Schema ProxyList inIds'
      return $ SomeFlow flow inSch' outSch
      where
        inIds = sc_ids inSch
        maybeInIds = vTransform renameMap inIds
        errRename = ErrMsg $
          "The step input ids don't match the referenced\
          \cwl tool or workflow's: " ++ stepName
        errMInIds = injectMaybe maybeInIds errRename
        renameMap = wfInputsAsHashmap wfins

        wfInputsAsHashmap :: [WflowStepInput] -> M.HashMap String String
        wfInputsAsHashmap = M.fromList . sourceAssocList
          where
            sourceAssocList :: [WflowStepInput] -> [(String,String)]
            sourceAssocList = map getSrcId
              where
                getSrcId :: WflowStepInput -> (String, String)
                getSrcId i = (wfstep_input_id i, wfstep_input_source i)

    -- | Make the output id have a name matching the reference
    -- in other fields. E.g., for a step with id "compile" and
    -- an output "binaryFile" rename it to "compile/binaryFile"
    -- to match whats writtien in the source fields in the 
    -- inputs of other steps.
    qualifyOutputs :: String -> SomeFlow -> SomeFlow
    qualifyOutputs prefix (SomeFlow flow inSch outSch) =
      SomeFlow flow inSch $ mapIds (vMap $ qualifySrc prefix) outSch

    -- | Qualify a string which appears as an output. If it is already
    -- qualified because it is from an inner workflow, remove the previous
    -- qualifier
    qualifySrc :: String -> String -> String
    qualifySrc prefix srcStr = case parseDestRef srcStr of
      Just dest -> prefix ++ "/" ++ dest
      Nothing -> prefix ++ "/" ++ srcStr



-- * Step 3: Use the Wiring and Schema utilities to write
-- 'convertWorkflow' and then 'convertCWLFile'
--------------------------------------------------------------------------------



convertWorkflow :: Workflow -> IOErrM SomeFlow
convertWorkflow wflow = do
  preorder <- injectErrM $ getWfPreOrder wflow
  let preordNoIns = removeInputNodes preorder
  let padPreorder = padWithInputs wfIns preordNoIns
  preorderedFlows <- convertToFlows padPreorder
  let wireLinPreord = injectErrM . wireLinearPhases
  oneFlow <- wireLinPreord preorderedFlows
  let reduceWfOut = reduceToWfOutput wfOuts oneFlow
  let err = SomeException $ ErrMsg $
       "Impossible error. Please report this\
       \ as a bug on the github repo."
  let reduceWfOut' = modErrM (const err) reduceWfOut
  convertedFlow <- injectErrM reduceWfOut'
  return convertedFlow

  where

    wfIns  = w_inputs wflow
    wfOuts = w_outputs wflow

    padWithInputs :: [InputParam] -> [[WfNode]] -> [[WfNode]]
    padWithInputs ins linearOrder = (map WfIn ins) : linearOrder

    -- If the input flow has an output schema that is larger
    -- than the output schema of the workflow, reduce it to
    -- that schema. Read about schema reduction in Wiring.hs
    reduceToWfOutput :: [WflowOutput] -> SomeFlow -> ErrM SomeFlow
    reduceToWfOutput wfouts (SomeFlow f inSch outSch) = do
      Some wfOutSchema <- return $ outputSchema wfouts
      Reduction red <- tryReduce outSch wfOutSchema
      return $ SomeFlow (f >>> step red) inSch wfOutSchema

      where
        outputSchema :: [WflowOutput] -> SomeSchema
        outputSchema [] = Some $ Schema nilProxyList VNil where
          nilProxyList :: ProxyList '[]
          nilProxyList = ProxyList
        outputSchema (x:xs) = case outputSchema xs of
          Some (Schema proxyList ids) ->
            case cwlTyToProxy $ wfout_type x of
              SomeTyProxy proxy ->
                let consedPList = consProxyList proxy proxyList in
                Some $ Schema consedPList (wfout_source x :.> ids)
                -- Note: we need the output id to be the source.
          where
            consProxyList :: TyProxy a -> ProxyList l -> ProxyList (a:l)
            consProxyList _ _ = ProxyList



convertCWLFile :: CWLFile -> IOErrM SomeFlow
convertCWLFile (CLToolFile tool) = injectErrM $ convertTool tool
convertCWLFile (WfFile wflow)    = convertWorkflow wflow

