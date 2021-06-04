{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (SomeException (..))
import Control.Exception.Safe (try)
import Control.Kernmantle.Error (tryE)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.CAS.ContentHashable (ContentHashable)
import Data.CAS.ContentStore (Content (..))
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Funflow
  ( Flow,
    RunFlowConfig (..),
    dockerFlow,
    ioFlow,
    runFlowWithConfig,
  )
import Funflow.Config (Configurable (..))
import qualified Funflow.Tasks.Docker as DE
import Parse
  ( parseMakeFile,
    parsecMakeFile,
    regularParse,
  )
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
    fromRelFile,
    parent,
    parseRelFile,
    parseAbsDir, 
    parseAbsFile,
    reldir,
    relfile,
    toFilePath,
    (</>),
  )
import Path.IO (getCurrentDir)
import System.Directory (getCurrentDirectory)
import System.Posix.Files (accessModes, createLink, setFileMode)
import Types

type Set = Set.Set

type Map = Map.Map

-- Other aliases
type FileName = String

type FileContent = String

-- | Main

--------------------------------------------------------------------------------

main :: IO ()
main = do
  perhapsMakeFile <- getValidMakeFile
  case perhapsMakeFile of
    Right (MFError errMsg) ->
      putStrLn $ "Invalid make file: \n" ++ errMsg
    Left mfile -> do
      cwd <- getCurrentDir
      let contentStore = cwd </> [reldir|makefiletest/store|]
          defGoalRule = defaultGoal mfile
          tfName = mkRuleTarNm defGoalRule
          build rule = buildTarget contentStore mfile rule >>= readByteString contentStore
          runCfg = runConfigWithoutFile contentStore
      putStrLn "Attempting build"
      result <- runFlowWithConfig runCfg (tryE @SomeException (ioFlow build)) defGoalRule :: IO (Either SomeException BS.ByteString)
      putStrLn "Build attempt complete"
      case result of
        Left ex ->
          putStrLn $
            "\n\nFailed, target failed: \n\n" ++ (show ex)
        Right (execContent :: BS.ByteString) -> do
          let outpath = (fromAbsDir cwd) ++ "/" ++ tfName
          BS.writeFile outpath execContent
          setFileMode outpath accessModes -- chmod +x
          putStrLn "\n\nSuccess, target executable made."

readByteString :: Path Abs Dir -> Content File -> IO BS.ByteString
readByteString store f = CS.withStore store (\s -> BS.readFile . fromAbsFile $ CS.contentPath s f)

-- | Extract valid makefile

--------------------------------------------------------------------------------
getValidMakeFile :: IO (Either MakeFile MFError)
getValidMakeFile = do
  readEither <- tryReadMakeFile
  case readEither of
    Left errMsg ->
      return $ Right $ MFError errMsg
    Right fileRead ->
      case parseMakeFile fileRead of
        Left errorMsg ->
          return $ Right $ MFError $ show errorMsg
        Right mkFile ->
          -- for now, ignoring checking it's valid
          return $ Left mkFile

tryReadMakeFile :: IO (Either String String)
tryReadMakeFile = do
  cwd <- getCurrentDirectory
  let makeFileLoc = cwd ++ "/Makefile"
  tryRead <- try $ readFile makeFileLoc
  case tryRead of
    Left (_ :: SomeException) -> return (Left makeFileLoc)
    Right file -> return (Right file)

-- | Strictly testing

--------------------------------------------------------------------------------
testMakeFileParse :: IO ()
testMakeFileParse = do
  Right mkfilestr <- tryReadMakeFile
  putStrLn "Testing make file parsing:"
  putStrLn "Readfile:"
  putStrLn mkfilestr
  putStrLn "Parsing:"
  print $ regularParse parsecMakeFile mkfilestr

-- | Building A Target

--------------------------------------------------------------------------------
-- Note: assuming the makefile is valid at this point!

buildTarget :: Path Abs Dir -> MakeFile -> MakeRule -> IO (Content File)
buildTarget storeRoot mkfile target@(MakeRule targetNm deps cmd) =
  let srcfiles = sourceFiles mkfile
      neededTargets = Set.toList $ Set.difference deps srcfiles
      neededSources = Set.toList $ deps `Set.intersection` srcfiles
      maybeFindDepRules = findRules mkfile neededTargets
   in case maybeFindDepRules of
        Nothing -> return (error "dependency rules missing")
        Just (depRules :: [MakeRule]) ->
          let depTargetActions = mapM (buildTarget storeRoot mkfile) depRules
              grabSrcsActions = mapM (readFile . ("./" ++))
           in do
                putStrLn ("needed targets: " ++ show neededTargets)
                putStrLn ("needed sources: " ++ show neededSources)
                putStrLn ("depRules: " ++ show depRules)
                guard (target `Set.member` (allrules mkfile))
                contentSrcFiles <- grabSrcsActions neededSources
                depFiles <- depTargetActions
                putStrLn ("depFiles: " ++ show depFiles)
                let fullSrcFiles = Map.fromList $ zip neededSources contentSrcFiles
                    runCfg = runConfigWithoutFile storeRoot
                compFile <- runFlowWithConfig runCfg (compileFile storeRoot) (targetNm, fullSrcFiles, depFiles, cmd)
                return compFile

findRules :: MakeFile -> [TargetFile] -> Maybe [MakeRule]
findRules MakeFile {allrules = rules} ts = do
  guard . null $ Set.difference tfileSet ruleTarNmSet
  let targetRules = Set.filter ((`Set.member` tfileSet) . mkRuleTarNm) rules
  return $ Set.toList targetRules
  where
    ruleTarNmSet = Set.map mkRuleTarNm rules
    tfileSet = Set.fromList ts

-- | Compiles a C file in a docker container.
compileFile :: Path Abs Dir -> Flow (TargetFile, Map.Map SourceFile String, [Content File], Command) (Content File)
compileFile root =
  let sandbox = "/sandbox"
  in ioFlow
    ( \(tf, srcDeps, tarDeps, cmd) -> do
        putStrLn ("Target: " ++ show tf)
        putStrLn ("Command: " ++ cmd)
        putStrLn ("srcDeps: " ++ show (Map.keysSet srcDeps))
        CS.withStore root (\s -> do
          let tarDepPaths = map (toFilePath . CS.contentPath s) tarDeps
          putStrLn ("tarDeps: " ++ show tarDepPaths))
        srcsInStore <- write2StoreRaw root srcDeps
        let inputFilesInStore = srcsInStore ++ tarDeps
        --inputDir <- mergeFilesRaw root (Just sandbox) inputFilesInStore
        inputDir <- mergeFilesRaw root Nothing inputFilesInStore
        let finalCmd = cmd ++ " -o " ++ tf
            scriptSrc =
              "#!/usr/bin/env bash\n\
              \echo arg1: $1\n\
              \cp $1/*.* $PWD && echo 'done copying' \n\
              \echo 'contents below:'\n\
              \ls $PWD\n"
                ++ finalCmd
                ++ "\n"
        putStrLn ("Final command: " ++ finalCmd)
        (compileItem, compileContent) <- writeExecStrRaw root (scriptSrc, [relfile|script.sh|])
        compilePath <- CS.withStore root (\s -> return (CS.contentPath s compileContent))
        putStrLn ("Compile script: " ++ show compilePath)
        relpathCompiledFile <- parseRelFile tf
        putStrLn ("relpathCompiledFile: " ++ show relpathCompiledFile)
        inputMount <- parseAbsDir sandbox
        let runCfg = runConfigWithoutFile root
            depIt = CS.contentItem inputDir :: CS.Item
            gccCfg =
              DE.DockerTaskConfig
                { DE.image = "gcc:7.3.0",
                  DE.command = "/script/script.sh",
                  --DE.args = [DE.Arg . Literal . Text.pack . toFilePath $ CS.itemRelPath depIt]
                  DE.args = [DE.Arg . Literal $ Text.pack sandbox]
                }
            scriptVol = DE.VolumeBinding {DE.item = compileItem, DE.mount = [absdir|/script/|]}
            --inputVol = DE.VolumeBinding {DE.item = depIt, DE.mount = [absdir|/sandbox/|]}
            inputVol = DE.VolumeBinding {DE.item = depIt, DE.mount = inputMount }
            taskIn = DE.DockerTaskInput {DE.inputBindings = [inputVol, scriptVol], DE.argsVals = mempty}
        (CS.:</> relpathCompiledFile) <$> runFlowWithConfig runCfg (dockerFlow gccCfg) taskIn
    )

-- Note: type SourceFile = String.
-- Note: TargetFile is the name of the file.

mergeFilesRaw :: Path Abs Dir -> Maybe String -> [CS.Content File] -> IO (CS.Content Dir)
mergeFilesRaw root relFolderOpt fs = CS.withStore root (\s -> merge s fs)
  where
    merge store files =
      let storeBased f = return $ CS.contentPath store f
          pathBased fldr f = 
            let grandparentPath = parent $ CS.contentPath store f
                parentPath = toFilePath grandparentPath ++ fldr
            in parseAbsFile $ parentPath ++ toFilePath (CS.contentFilename f)
          getAbs Nothing = storeBased
          getAbs (Just "") = storeBased
          getAbs (Just ('/':folder)) = pathBased folder
          getAbs (Just folder) = pathBased folder
          --absFiles = map (CS.contentPath store) files
          linkIn d = mapM_ (\f -> do
            let realPath = toFilePath f
                linkPath = toFilePath $ d </> filename f
            putStrLn ("Real: " ++ realPath)
            putStrLn ("Link: " ++ linkPath)
            createLink realPath linkPath
            )
       in do
         absFiles <- mapM (getAbs relFolderOpt) files :: IO [Path Abs File]
         CS.All <$> CS.putInStore store RC.NoCache (\_ -> return (error "uh-oh!")) linkIn absFiles

writeExecStrRaw :: Path Abs Dir -> (String, Path Rel File) -> IO (CS.Item, CS.Content File)
writeExecStrRaw d = putInStoreAtRaw d (\p x -> writeFile (fromAbsFile p) x >> setFileMode (fromAbsFile p) accessModes)

write2StoreRaw :: Path Abs Dir -> Map.Map FileName FileContent -> IO [Content File]
write2StoreRaw store stuff =
  let ioFixSrcFileData fn fc = (\fc' -> (fc, fc')) <$> parseRelFile fn
      write1 fn fc = do
        (a, p) <- ioFixSrcFileData fn fc
        (_, f) <- putInStoreAtRaw store (writeFile . fromAbsFile) (a, p)
        return f
   in mapM (uncurry write1) (Map.toList stuff)

putInStoreAtRaw ::
  (ContentHashable IO a, Typeable t) =>
  Path Abs Dir ->
  (Path Abs t -> a -> IO ()) ->
  (a, Path Rel t) ->
  IO (CS.Item, CS.Content t)
putInStoreAtRaw root put (a, p) =
  CS.withStore
    root
    ( \store -> do
        item <- CS.putInStore store RC.NoCache (\_ -> return ()) (\d a -> put (d </> p) a) a
        return (item, item CS.:</> p)
    )

runConfigWithoutFile :: Path Abs Dir -> RunFlowConfig
runConfigWithoutFile d = RunFlowConfig {storePath = d, configFile = Nothing}
