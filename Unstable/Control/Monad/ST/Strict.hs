module Unstable.Control.Monad.ST.Strict (module S) where

import Control.Monad.ST.Strict as S
import Unstable.Control.Monad.Trans

instance HasBaseMonad (ST s) (ST s) where
  inBase = id




