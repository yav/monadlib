module Unstable.Control.Monad.ST.Lazy (module S) where

import Control.Monad.ST.Lazy as S
import Unstable.Control.Monad.Trans

instance HasBaseMonad (ST s) (ST s) where
  inBase = id




