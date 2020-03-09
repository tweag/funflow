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


{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-|
Module      : Job
Description : Convert 'Job's to generative flows.

This module converts our internal representation of a 'Job' into
an existential type that creates an input record. This input record is
represented by (1) flow that takes no arguments and produces a heterogeneous
list and (2) a list of the same length of ids.
-}


module Control.Funflow.CWL.Convert.Job
  ( convertJob
  , SomeJob (..)
  ) where


-- Funflow
import Control.Arrow
import           Control.Funflow
import Data.CAS.ContentStore ( Content (..) )
import Data.CAS.ContentHashable
  ( FileContent (..)
  , DirectoryContent (..)
  )

-- Regular Imports
import Path
  ( Path, Rel, File, parseRelFile
  , parseRelDir, Dir, (</>) )
import Path.IO (getCurrentDir)
import Data.Proxy
import Data.Typeable
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import System.FilePath.Posix ( takeFileName )
import System.Directory

-- Internal
import Control.Funflow.CWL.Types
import Control.Funflow.CWL.Util.CWLUtil
import Data.HList
import Data.Vec
import Data.ErrorMonad
import Data.Existentials





-- * Converting a Job
--------------------------------------------------------------------------------

-- Note:
-- type Job = (M.HashMap T.Text CWLObject)

-- | This holds a representation of an input record. It holds a generative flow
-- -- a flow that writes the needed files to the store and produces the input
-- heterogeneous list and along side this generative flow, a list of ids for
-- each element of the heterogeneous list.
data SomeJob where
  SomeJob :: (QEmpty l, TraversibleL l) =>
    { someInFlow :: HList '[] ==> HList l
    , someInSch :: Schema l
    } -> SomeJob

-- | Convert a job into a generative flow. A generative flow is one that writes
-- the needed files to the store and produces the input heterogeneous list.  We
-- assume the current directory of running this function is the one in which
-- the job file resides.
convertJob :: Job -> IOErrM SomeJob
convertJob = convJob . M.toList
 where

    convJob :: [(T.Text, CWLObject)] -> IOErrM SomeJob
    convJob  [] = return $ SomeJob (injectA HNil) (Schema ProxyList VNil)
    convJob ((id',obj): xs) = do
        SomeJob recurJob (inSch :: Schema l) <- convJob xs
        let inIds = sc_ids inSch
        let inTys = sc_types inSch
        CWLData (oneTy, proxyTy) <- getCWLData obj
        let newJobFlow = enlargeJobF oneTy recurJob
        let inIds' = (T.unpack id') :.> inIds
        let inTys' = pLCons proxyTy inTys
        let inSch' = Schema inTys' inIds'
        return $ SomeJob newJobFlow inSch'

    enlargeJobF :: (Typeable t, Show t, TraversibleL l) =>
      (HList '[] ==> t)       ->
      (HList '[] ==> HList l) ->
      (HList '[] ==> HList (t:l))
    enlargeJobF getT job = proc _ -> do
      jobList <- job -< HNil
      t <- getT -< HNil
      returnA -< (t :> jobList)

    pLCons :: TyProxy t -> ProxyList l -> ProxyList (t:l)
    pLCons _ _ = ProxyList

-- | This is a existential around a generative flow for a
-- single type.
data SomeCWLData where
  CWLData :: (Typeable t, Show t) =>
    (HList '[] ==> t, TyProxy t) -> SomeCWLData

-- | Given a single 'CWLObject', produce a generative flow for it.
getCWLData :: CWLObject -> IOErrM SomeCWLData
getCWLData (CWLstr s)     = return $ CWLData (injectA s, Proxy)
getCWLData (CWLbool b)    = return $ CWLData (injectA b, Proxy)
getCWLData (CWLnum n)     = return $ CWLData (injectA n, Proxy)
getCWLData (CWLfile path) = do
  canRead <- lift $ fileReadable path
  let err = ErrMsg $ "The job specifies a non-existant\
                      \or unreadable file: " ++ path
  guardIOErrM canRead err
  return $ CWLData (getCF path, Proxy)
-- TODO: Check dir readable and exists
getCWLData (CWLdir path) = return $ CWLData (getCDir path, Proxy)
getCWLData (CWLmaybe (maybeObj,cwlTy)) = case maybeObj of
  Nothing -> do
    SomeMaybe m <- return $ castNothing cwlTy
    return $ CWLData (injectA m, Proxy)
  Just obj -> do
    CWLData (genObj, p) <- getCWLData obj
    let newObj = genMaybe genObj
    let newProxy = proxyMaybe p
    let err = ErrMsg $ "Cannot convert job file. Optional\
                     \ input and type don't match: \n" ++
                       show (maybeObj, cwlTy)
    guardIOErrM (tyMatch p cwlTy) err
    return $ CWLData (newObj, newProxy)

  where

    genMaybe :: (HList '[] ==> t) -> (HList '[] ==> Maybe t)
    genMaybe flow = proc _ -> do
      t <- flow -< HNil
      returnA -< (Just t)

    castNothing :: CWLType -> SomeMaybe
    castNothing ty = case cwlTyToProxy ty of
      SomeTyProxy (_ :: Proxy t) ->
        SomeMaybe (Nothing :: Maybe t)

    proxyMaybe :: (Typeable t, Show t) =>
      TyProxy t -> TyProxy (Maybe t)
    proxyMaybe Proxy = Proxy

getCWLData (CWLarr ([], cwlTy)) = do
  SomeTyProxy p <- return $ cwlTyToProxy cwlTy
  return $ CWLData (injectA [], proxyList p)
  where
    proxyList :: (Typeable t, Show t) =>
      TyProxy t -> TyProxy [t]
    proxyList _ = Proxy

getCWLData (CWLarr ((x:xs), cwlTy)) = do
  let recurArr = CWLarr (xs, cwlTy)
  CWLData (genXs, xslistTy) <- getCWLData recurArr
  CWLData (genX, xTy) <- getCWLData x
  let xlistTy = proxyList xTy
  let err = ErrMsg $ "Array in job file is not mono\
            \-typed. Look at element: \n" ++ show x
  let maybeMatch = proveTyMatch xlistTy xslistTy
  Refl <- injectMaybe' maybeMatch err
  let newObj = consFlow genXs genX
  return $ CWLData (newObj, xslistTy)

  where

    proxyList :: (Typeable t, Show t) =>
      TyProxy t -> TyProxy [t]
    proxyList _ = Proxy

    consFlow :: (Typeable t, Show t) => 
      (HList '[] ==> [t]) ->
      (HList '[] ==> t) ->
      (HList '[] ==> [t])
    consFlow xsFlow xFlow = proc _ -> do
      ts <- xsFlow -< HNil
      t <- xFlow -< HNil
      returnA -< (t : ts)


tyMatch :: Typeable t => TyProxy t -> CWLType -> Bool
tyMatch (_ :: Proxy t) ty = case cwlTyToProxy ty of
  SomeTyProxy (_ :: Proxy t') -> isJust (eqT :: Maybe (t :~: t'))
  where
    isJust Nothing = False
    isJust _ = True


proveTyMatch :: (Typeable t, Typeable t') =>
  Proxy t -> Proxy t' -> Maybe (t :~: t')
proveTyMatch _ _ = eqT


-- * Large Helper Functions and Data
--------------------------------------------------------------------------------

-- Not meant to be an abstraction;
-- this just reduces line length:
type GenMany a = HList '[] ==> [a]

-- | Given a generative flow for @[a]@ and an existential generative flow for a
-- single type, see if that type is @a@ and if so, add it to the first
-- generative flow.
consCWLObj :: Typeable a =>
  GenMany a -> SomeCWLData -> ErrM (GenMany a)
consCWLObj flowToAs (CWLData (someObjFlow, objTy)) = do
  let expectedTy = getGenManyProxy flowToAs
  let err = ErrMsg $ "The input array is not all of one \
        \type. We expected it to be of type " ++ show expectedTy
  let maybeRefl = compProxy expectedTy objTy
  Refl <- injectMaybe maybeRefl err
  return $
    proc _ -> do
      as <- flowToAs -< HNil
      a <- someObjFlow -< HNil
      returnA -< (a:as)
 where

    getGenManyProxy :: GenMany a -> TyProxy a
    getGenManyProxy _ = Proxy


    compProxy :: (Typeable t, Typeable t') =>
      TyProxy t -> TyProxy t' -> Maybe (t :~: t')
    compProxy (_ :: Proxy t) (_ :: Proxy t') =
      eqT :: Maybe (t :~: t')


-- | Given many Flows that work on an input of type @a@, make one flow that
-- takes a single @a@ and makes a list of @b@s by applying each flow.
flowSplit :: [a ==> b] -> a ==> [b]
flowSplit [] = proc _ -> do
  returnA -< []
flowSplit (f:fs) = proc a -> do
  bs <- flowSplit fs -< a
  b <- f -< a
  returnA -< (b:bs)



data SomeMaybe where
  SomeMaybe :: (Typeable t, Show t) =>
    Maybe t -> SomeMaybe

data SomeList where
  SomeList :: (Typeable t, Show t) =>
    [t] -> SomeList

-- | Inject a single type into a generative flow.
injectA :: (Typeable t, Show t) => t -> HList '[] ==> t
injectA t = proc _ -> do
  returnA  -< t




-- * Dealing with things that need to be put into the store
--------------------------------------------------------------------------------
type FileName = String
type CF = Content File


getCF :: FilePath -> HList '[] ==> CF
getCF path = proc _ -> do
  let fileNm = takeFileName path
  relNm <- toRelFile -< fileNm

  cwd <- stepIO $ const getCurrentDir -< ()
  relPath <- toRelFile -< path
  let absPath = cwd </> relPath
  let fileCont = FileContent absPath

  contentFile <- copyFileToStore -< (fileCont, relNm)
  returnA -< contentFile
  where

    toRelFile :: String ==> Path Rel File
    toRelFile = stepIO parseRelFile


getCDir :: FilePath -> HList '[] ==> Content Dir
getCDir path = proc _ -> do
  cwd <- stepIO $ const getCurrentDir -< ()
  relPath <- toRelDir -< path
  let absPath = cwd </> relPath
  let dirCont = DirectoryContent absPath

  contentDir <- copyDirToStore -< (dirCont, Nothing)
  returnA -< contentDir
  where

    toRelDir :: String ==> Path Rel Dir
    toRelDir = stepIO parseRelDir



fileReadable :: FilePath -> IO Bool
fileReadable path = do
  exists <- doesFileExist path
  permissions <- getPermissions path
  let isReadable = readable permissions
  return $ isReadable && exists



