{-
 - Entry point to Funflow
 -}
module Funflow
  ( module Funflow.Flow,
    (>>>),
    caching,
    runFlow,
  )
where

import Control.Arrow ((>>>))
import Control.Kernmantle.Caching (caching)
import Funflow.Flow
import Funflow.Run
  ( runFlow,
  )
