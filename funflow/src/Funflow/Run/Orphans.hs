{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Funflow.Run.Orphans where

import Control.Exception.Safe (Exception)
import Data.CAS.ContentHashable (ContentHashable)
import Docker.API.Client (ContainerSpec, DockerClientError)

instance ContentHashable IO ContainerSpec

instance ContentHashable IO DockerClientError

instance Exception DockerClientError