-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the resumption monad.  Is this terribly useful?
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.Resume (Resume, runResume, foldResume, module T) where

import Unstable.Control.Monad.Identity 
import qualified Unstable.Control.Monad.ResumeT as R
import Unstable.Control.Monad.Trans as T     

type Resume   = R.ResumeT Identity


foldResume    :: (a -> b) -> (b -> b) -> Resume a -> b
foldResume value delay m  
              = runIdentity (R.foldResume v d m)
  where v x   = return (value x)
        d x   = return (delay (runIdentity x))


runResume     :: Resume a -> a
runResume m   = runIdentity (R.runResume m)



