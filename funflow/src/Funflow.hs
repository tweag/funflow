{-
 - Entry point to Funflow
 -}
module Funflow
  ( module Funflow.Flow,
    module Funflow.Run,
    (>>>),
    caching,
  )
where

import Control.Arrow ((>>>))
import Control.Kernmantle.Caching (caching)
import Funflow.Flow
import Funflow.Run
