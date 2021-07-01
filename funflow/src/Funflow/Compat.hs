{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Aliases which behave similarly to their funflow 1.x counterparts.
module Funflow.Compat where

import Data.CAS.ContentHashable (ContentHashable)
import qualified Data.CAS.ContentStore as CS
import Data.Store (Store)
import Funflow (Flow, caching)
import Funflow.Flow (dockerFlow, getDirFlow, ioFlow, putDirFlow)
import Funflow.Tasks.Docker (DockerTaskConfig, DockerTaskInput)
import Path (Abs, Dir, Path)

stepIO :: (i -> IO o) -> Flow i o
stepIO = ioFlow

stepIO' :: (Show i, ContentHashable IO i, ContentHashable IO ident, Store o) => ident -> (i -> IO o) -> Flow i o
stepIO' ident = caching ident . ioFlow

docker :: DockerTaskConfig -> Flow DockerTaskInput CS.Item
docker = dockerFlow

putInStore :: Flow (Path Abs Dir) CS.Item
putInStore = putDirFlow

getFromStore :: Flow CS.Item (Path Abs Dir)
getFromStore = getDirFlow
