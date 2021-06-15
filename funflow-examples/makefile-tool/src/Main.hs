{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Arrow
import Control.Exception (SomeException (..))
import Control.Kernmantle.Caching (caching)
import Control.Kernmantle.Error (tryE)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.CAS.ContentHashable (ContentHashable)
import Data.CAS.ContentStore (Content (..))
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Funflow
  ( Flow,
    RunFlowConfig (..),
    dockerFlow,
    ioFlow,
    pureFlow,
    runFlowWithConfig,
  )
import qualified Funflow.Tasks.Docker as DE
import Parse (getValidMakeFile)
import Path
  ( Abs,
    Dir,
    File,
    Path,
    Rel,
    absdir,
    filename,
    fromAbsDir,
    fromAbsFile,
    parseRelFile,
    parseAbsDir,
    reldir,
    relfile,
    toFilePath,
    (</>),
  )
import Path.IO (getCurrentDir)
import System.Posix.Files (accessModes, createLink, setFileMode)
import Types

type Set = Set.Set
type Map = Map.Map
type FileName = String
type FileContent = String

main :: IO ()
main = do
  perhapsMakeFile <- getValidMakeFile Nothing
  case perhapsMakeFile of
    Right (MFError errMsg) -> putStrLn $ "Invalid make file:\n" ++ errMsg
    Left mfile -> do
      cwd <- getCurrentDir
      let contentStore = cwd </> [reldir|makefiletest/store|]
          defGoalRule = defaultGoal mfile
          runCfg = getRunConfigWithoutFile contentStore
      putStrLn ("Attempting build:\n" ++ show defGoalRule)
      result <- runFlowWithConfig runCfg (tryE @SomeException (buildTarget contentStore mfile defGoalRule)) () :: IO (Either SomeException (Path Abs File))
      putStrLn "Build attempt complete"
      case result of
        Left ex -> putStrLn $ "\n\nFailed, target failed:\n\n" ++ (show ex)
        Right execFile -> do
          -- Build succeeded; write executable to target location and set exec bit.
          let outpath = fromAbsDir cwd ++ "/" ++ mkRuleTarNm defGoalRule
          BS.readFile (fromAbsFile execFile) >>= BS.writeFile outpath
          setFileMode outpath accessModes -- chmod +x
          putStrLn "\n\nSuccess, target executable made."

-- | Building A Target
--------------------------------------------------------------------------------
-- Note: assuming the makefile is valid at this point!
buildTarget :: Path Abs Dir -> MakeFile -> MakeRule -> Flow () (Path Abs File)
buildTarget storeRoot mkfile target@(MakeRule targetNm deps cmd) = let
   srcfiles = sourceFiles mkfile
   -- What must be built is the collection of non-source dependencies.
   neededTargets = Set.toList $ Set.difference deps srcfiles
   -- We only need the sources that are pertinent for the current rule's dependencies.
   neededSources  = Set.toList $ deps `Set.intersection` srcfiles
   maybeFindDepRules = findRules mkfile neededTargets
 in case maybeFindDepRules of
   Nothing -> failNow
   Just (depRules :: [MakeRule]) -> let
     -- found rules for dependencies to be built for current rule
       grabSrcsActions = mapM (readFile . ("./" ++))
     in proc _ -> do
       msgFlow ("Current rule: " ++ show target) -< ()
       () <- failGuardFlow -< (target `Set.member` (allrules mkfile))
       -- Read content from source files.
       contentSrcFiles <- (ioFlow grabSrcsActions) -< neededSources
       -- Result from building each dependency.
       depFiles <- flowJoin [Id{ unId = buildTarget storeRoot mkfile r } | r <- depRules] -< (replicate (length depRules) ())
       let fullSrcFiles = Map.fromList $ zip neededSources contentSrcFiles
       -- Compile the target of the current rule.
       compFile <- caching targetNm (compileFile storeRoot) -< (targetNm, fullSrcFiles, depFiles, cmd)
       returnA -< compFile


-- | Compiles a C file in a docker container.
compileFile :: Path Abs Dir -> Flow (TargetFile, Map.Map SourceFile String, [Path Abs File], Command) (Path Abs File)
compileFile root = proc (tf, srcDeps, tarDeps, cmd) -> do
  relpathCompiledFile <- (ioFlow parseRelFile) -< tf
  srcsInStore <- write2Store root -< srcDeps
  let inputFilesInStore = srcsInStore ++ tarDeps
  inputDir <- mergeFiles root -< inputFilesInStore
  let finalCmd = cmd ++ " -o " ++ tf
      scriptSrc =
        "#!/usr/bin/env bash\n\
        \cp $1/*.* $PWD\n"
          ++ finalCmd
          ++ "\n"
  _ <- ioFlow (\msg -> putStrLn ("Current command: " ++ msg)) -< finalCmd
  (compItem, _) <- writeExecutableString root -< (scriptSrc, [relfile|script.sh|])
  inMnt <- ioFlow parseAbsDir -< "/sandbox"
  taskIn <- pureFlow buildTaskInput -< (inputDir, compItem, inMnt)
  resDir <- dockerFlow dockConf -< taskIn
  resFile <- ioFlow (ioContentPath root) -< (resDir CS.:</> relpathCompiledFile)
  returnA -< resFile
    where dockConf = DE.DockerTaskConfig{ DE.image = "gcc:7.3.0", DE.command = "/script/script.sh", DE.args = ["/sandbox"] }
          buildTaskInput (indir, compItem, mnt) = DE.DockerTaskInput{
            DE.inputBindings = [
              DE.VolumeBinding {DE.item = compItem, DE.mount = [absdir|/script/|]},
              DE.VolumeBinding{DE.item = CS.contentItem indir, DE.mount = mnt}
            ],
            DE.argsVals = mempty
          }


----------------------------------------------------------------------------------------
-- These are helpers for overcoming challenge of typelevel polymorphism in kernmantle.
-- In particular, this is for beating "illegal polymorphic type..."
-- related to impredicative polymorphism
----------------------------------------------------------------------------------------

-- Wrap a flow so that we can overcome impredicative polymporphism.
-- See SO (answer by Jon Purdy): https://stackoverflow.com/a/56449258
newtype Id a b = Id{ unId :: (Flow a b) }

-- "Merge" a a collection of "atomic" flows into a single flow from one collection to another.
-- Note that order should not matter, else the result should be reversed.
-- In particular, we process the flow/input pairs left-to-right, but build the result right-to-left.
flowJoin :: [Id a b] -> Flow [a] [b]
flowJoin [] = pureFlow (\_ -> [])
flowJoin ff@(f:fs) = proc aa@(a:as) -> do
  -- Enforce dimensional match between flows and inputs.
  () <- failGuardFlow -< (length ff == length aa)
  -- "Run" the "current" flow
  b <- unId f -< a
  -- Recurse on the remaining flows and inputs.
  bs <- flowJoin fs -< as
  returnA -< (b:bs)


-------------------------------------------------------------------------------
-- Application-related
-------------------------------------------------------------------------------

-- For each of a collection of files, create a link in the store root.
mergeFilesRaw :: Path Abs Dir -> [Path Abs File] -> IO (Content Dir)
mergeFilesRaw root fs = CS.withStore root (\s -> merge s fs)
    where merge store files = let linkIn d = mapM_ ( \f -> createLink (toFilePath f) (toFilePath $ d </> filename f) )
                              in CS.All <$> CS.putInStore store RC.NoCache (\_ -> return (error "uh-oh!")) linkIn files

-- Flow version of linking a collection of files into the store root
mergeFiles :: Path Abs Dir -> Flow [Path Abs File] (Content Dir)
mergeFiles root = ioFlow (mergeFilesRaw root)

-- Write to a file at given relpath, in the store at the given root.
writeExecutableString :: Path Abs Dir -> Flow (String, Path Rel File) (CS.Item, Content File)
writeExecutableString root = ioFlow (putInStoreAt root (\p x -> let p' = fromAbsFile p in writeFile p' x >> setFileMode p' accessModes))

-- Find the collection of rules in the given makefile for the given collection of targets.
findRules :: MakeFile -> [TargetFile] -> Maybe [MakeRule]
findRules MakeFile{ allrules = rules } targets = do
  guard . null $ Set.difference tfileSet ruleTarNmSet    -- check that each each target has a rule.
  -- We're only interested in the rules for the given set of targets
  let targetRules = Set.filter ((`Set.member` tfileSet) . mkRuleTarNm) rules
  return $ Set.toList targetRules
  where
    ruleTarNmSet = Set.map mkRuleTarNm rules
    tfileSet = Set.fromList targets


-------------------------------------------------------------------------------
-- Storage-related
-------------------------------------------------------------------------------
-- Use a given function to store a given value at given relative path, within store rooted at given path.
putInStoreAt ::
  (ContentHashable IO a, Typeable t) =>
  Path Abs Dir ->
  (Path Abs t -> a -> IO ()) ->
  (a, Path Rel t) ->
  IO (CS.Item, CS.Content t)
putInStoreAt root put (a, p) =
  CS.withStore
    root
    ( \store -> do
        item <- CS.putInStore store RC.NoCache (\_ -> return ()) (\d x -> put (d </> p) x) a
        return (item, item CS.:</> p)
    )

-- Write each content to file with associated name, within store rooted at given path.
write2Store :: Path Abs Dir -> Flow (Map.Map FileName FileContent) [Path Abs File]
write2Store root =
  let ioFixSrcFileData (x,y) = (\y' -> (y,y')) <$> parseRelFile x
  in ioFlow (\files -> mapM (\x -> do
    y <- ioFixSrcFileData x
    (_, z) <- putInStoreAt root (writeFile . fromAbsFile) y
    fp <- ioContentPath root z
    return fp
  ) (Map.toList files) )

-- Get the path to content in the store.
ioContentPath :: Path Abs Dir -> Content t -> IO (Path Abs t)
ioContentPath root x = CS.withStore root (\s -> return (CS.contentPath s x))


-------------------------------------------------------------------------------
-- General flow-related
-------------------------------------------------------------------------------
getRunConfigWithoutFile :: Path Abs Dir -> RunFlowConfig
getRunConfigWithoutFile d = RunFlowConfig {storePath = d, configFile = Nothing}

-- Flow that fails iff given flag is False.
failGuardFlow :: Flow Bool ()
failGuardFlow = proc test -> do
  case test of
    True -> returnA -< ()
    False -> do
      () <- failNow -< ()
      returnA -< ()

-- Degenerate flow failing with fixed, basic error message
failNow :: Flow () a
failNow = proc _ -> do ioFlow err -< ()
  where err () = do (error "error")

-- Flow that accepts message and fails with that as the error message
failWith :: Flow String a
failWith = ioFlow (\msg -> do error msg)

-- Degenerate flow that prints the given message
msgFlow :: String -> Flow () ()
msgFlow msg = ioFlow (\_ -> putStrLn msg)
