-- | Composable computational workflows.
--   See https://github.com/tweag/funflow2
--
--   This module re-exports other modules for conveniency.
module Funflow
  ( module Funflow.Flow,
    module Funflow.Run,
    (>>>),
    caching,
    tryE,
    throwE,
  )
where

import Control.Arrow ((>>>))
import Control.Kernmantle.Caching (caching)
import Control.Kernmantle.Error (throwE, tryE)
import Funflow.Flow
import Funflow.Run
