{-# LANGUAGE Arrows              #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- Funflow Imports
import           Control.Arrow
import           Control.Arrow.Free
import           Control.Exception.Safe ( try )
import           Control.Funflow
import           Control.Funflow.External
import qualified Control.Funflow.ContentStore as CS
import Control.Funflow.ContentStore ( Content (..) )
import Control.Exception ( Exception (..), SomeException (..) )
import qualified Control.Funflow.External.Docker as Docker

-- Library Imports
import Control.Monad ( guard )
import Path.IO ( getCurrentDir )
import Path ( Path, Rel, File
            , reldir, Dir, relfile
            , parseRelFile, fromAbsDir
            , fromAbsFile, (</>)
            )
import System.IO ( IO )
import System.Directory ( getCurrentDirectory )
import System.Posix.Files ( setFileMode, accessModes )
import Data.Traversable ( sequence )
import qualified Data.ByteString as BS

-- Internal Imports
import Parse ( regularParse, parseMakeFile
             , parsecMakeFile )
import Types






import qualified Data.Text as T



-- Data Structures
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
type Set = Set.Set
type Map = Map.Map


-- | Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    perhapsMakeFile <- getValidMakeFile
    case perhapsMakeFile of
      Right (MFError errMsg) -> 
        putStrLn $ "Invalid make file: \n" ++ errMsg
      Left mfile -> do
        let defGoalRule = defaultGoal mfile
        let tfName = mkRuleTarNm defGoalRule
        cwd <- getCurrentDir
        let contentStore = cwd </> [reldir|makefiletest/store|]
        r <- withSimpleLocalRunner contentStore $ \run ->
          run ((buildTarget mfile defGoalRule) >>> readByteString) ()
        case r of
          Left errorMsg ->
            putStrLn $ 
              "\n\nFailed, target failed: \n\n" ++ displayException errorMsg
          Right (execContent :: BS.ByteString) -> do
            let outpath = (fromAbsDir cwd) ++ "/" ++ tfName
            BS.writeFile outpath execContent
            setFileMode outpath accessModes -- chmod +x
            putStrLn "\n\nSuccess, target executable made."


readByteString :: Content File ==> BS.ByteString
readByteString = getFromStore $ BS.readFile . fromAbsFile


-- | Extract valid makefile
--------------------------------------------------------------------------------

getValidMakeFile :: IO (Either MakeFile MFError)
getValidMakeFile = do
  tryRead <- readMakeFileMaybe
  case tryRead of
    Nothing ->
      return $ Right $ MFError $ "`cwd`/makefile does not exist"
    Just fileRead ->
      case parseMakeFile fileRead of
        Left errorMsg ->
          return $ Right $ MFError $ show errorMsg
        Right mkFile ->
          -- for now, ignoring checking it's valid
          return $ Left mkFile


readMakeFileMaybe :: IO (Maybe String)
readMakeFileMaybe = do
  cwd <- getCurrentDirectory
  let makeFileLoc = cwd ++ "/Makefile"
  tryRead <- try $ readFile makeFileLoc
  case tryRead of
    Left (_ :: SomeException) -> return Nothing
    Right file -> return (Just file)


testMakeFileParse :: IO ()
testMakeFileParse = do
  Just mkfilestr <- readMakeFileMaybe
  putStrLn "Testing make file parsing:"
  putStrLn "Readfile:"
  putStrLn mkfilestr
  putStrLn "Parsing:"
  putStrLn $ show $ regularParse parsecMakeFile mkfilestr


-- | Building A Target
--------------------------------------------------------------------------------

-- Note: assuming the makefile is valid at this point!

buildTarget :: MakeFile -> MakeRule -> (() ==> (Content File))
buildTarget mkfile target@(MakeRule targetNm deps cmd) = let
   srcfiles = sourceFiles mkfile
   neededTargets = Set.toList $ deps `Set.difference` srcfiles
   neededSources  = Set.toList $ deps `Set.intersection` srcfiles
   maybeFindDepRules = findRules mkfile neededTargets
 in case maybeFindDepRules of
   Nothing -> proc _ -> do
     () <- failNow -< ()
     returnA -< (error "dependency rules missing")
   Just (depRules :: [MakeRule]) -> let
       depTargetFlows = map (buildTarget mkfile) depRules
       countDepFlows = length depTargetFlows
       grabSources srcs = sequence $ map (readFile . ("./" ++)) srcs
       grabSrcsFlow = stepIO grabSources
     in proc _ -> do
       -- really cool application: no repeated work here!
       -- a powerful dynamic programming tool
       () <- guardFlow -< (target `Set.member` (allrules mkfile)) 
       contentSrcFiles <- grabSrcsFlow -< neededSources
       let fullSrcFiles = Map.fromList $ zip neededSources contentSrcFiles
       depFiles <- flowJoin depTargetFlows -< (replicate countDepFlows ())
       compiledFile <- compileFile -< (targetNm, fullSrcFiles, depFiles,cmd)
       returnA -< compiledFile


findRules :: MakeFile -> [TargetFile] -> Maybe [MakeRule]
findRules MakeFile {allrules = rules} ts = do
  guard ((tfileSet Set.\\ ruleTarNmSet) == Set.empty)
  let targetRules = Set.filter ((`Set.member` tfileSet) . mkRuleTarNm) rules
  return $ Set.toList targetRules
  where
    ruleTarNmSet = Set.map mkRuleTarNm rules
    tfileSet = Set.fromList ts


-- | Compiles a C file in a docker container.
compileFile ::
  (TargetFile, Map SourceFile String, [Content File], Command) ==> (Content File)
compileFile = proc (tf, srcDeps, tarDeps, cmd) -> do
  srcsInStore <- writeToStore -< srcDeps
  let inputFilesInStore = srcsInStore ++ tarDeps
  inputDir <- mergeFiles -< inputFilesInStore
  let scriptSrc = "#!/usr/bin/env bash\n\
                  \cd /input/deps\n" ++ 
                  cmd  ++ " -o /output/" ++ tf
  compileScript <- writeExecutableString -< (scriptSrc, [relfile|script.sh|])
  compiledFile <- dockerFlow -< (inputDir,compileScript)
  relpathCompiledFile <- flowStringToRelFile -< tf
  returnA -< (compiledFile :</> relpathCompiledFile)
    where
      dockerFlow :: (Content Dir, Content File) ==> CS.Item
      dockerFlow = docker dockerConfFn
      dockerConfFn (depDir, compileScript) = Docker.Config
        { Docker.image = "gcc"
        , Docker.optImageID = Just "7.3.0"
        , Docker.input = Docker.MultiInput inputs
        , Docker.command = "/input/script/script.sh"
        , Docker.args = []
        }
        where
          inputs = Map.fromList [scriptInput, depInput]
          mkInputPath = IPItem . CS.contentItem
          scriptInput = ("script", mkInputPath compileScript)
          depInput = ("deps", mkInputPath depDir)


-- Note: type SourceFile = String.
-- Note: TargetFile is the name of the file.


flowStringToRelFile :: String ==> Path Rel File
flowStringToRelFile = stepIO parseRelFile

type FileName = String
type FileContent = String

writeToStore :: (Map FileName FileContent) ==> [Content File]
writeToStore = proc files -> do
  let listfiles = Map.toList files
  filesWithPaths <- mapA flowfixSrcFileData -< listfiles
  contents <- mapA writeString -< filesWithPaths
  returnA -< contents


flowfixSrcFileData :: (String, String) ==> (String, Path Rel File)
flowfixSrcFileData = stepIO ioFixSrcFileData


ioFixSrcFileData :: (FileName, FileContent) -> IO (FileContent, Path Rel File)
ioFixSrcFileData (x,y) = do
  path <- parseRelFile x
  -- Note: parseRelFile :: FileName -> IO Path Rel File.
  return (y,path)









-- Figuring out the conversion

runSimpFlow :: IO ()
runSimpFlow = do
  cwd <- getCurrentDir
  let contentStore = cwd </> [reldir|makefiletest/store|]
  r <- withSimpleLocalRunner contentStore $ \run ->
    run (simpleFlow >>> readString) ("test string", "fileTest")
  case r of
    Left errMsg -> putStrLn $ "\n Failed: " ++ displayException errMsg
    Right fileContent -> putStrLn $ "\n File content : \n" ++ fileContent


simpleFlow :: (String, String) ==> (Content File)
simpleFlow = proc (str, name) -> do
  item <- external mkExternal -< (str, name)
  relfilePath <- flowStringToRelFile -< name
  returnA -< (item :</> relfilePath)

mkExternal :: (String, String) -> ExternalTask
mkExternal (str, name) = ExternalTask
  { _etCommand = T.pack ("printf " ++ (show str) " > ")
  , _etParams = [outParam]
  , _etWriteToStdOut = True
  }

















-- | Library Functions
--------------------------------------------------------------------------------

flowJoin :: [a ==> b] -> ([a] ==> [b])
flowJoin [] = step (\_ -> [])
flowJoin ff@(f:fs) = proc aa@(a:as) -> do
  () <- guardFlow -< (length ff == length aa)
  (b,bs) <- f  *** (flowJoin fs) -< (a, as)
  returnA -< (b:bs)


guardFlow :: Bool ==> ()
guardFlow = proc test -> do
  case test of
    True -> returnA -< ()
    False -> do
      () <- failNow -< ()
      returnA -< ()


failNow :: () ==> ()
failNow = proc _ -> do
      (stepIO err) -< ()


err :: () -> IO ()
err () = do
  error "error"


printFlow :: String ==> ()
printFlow = proc s -> do
  () <- stepIO putStrLn -< s
  returnA -< ()
