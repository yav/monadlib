module Unstable.Control.Monad.Reader (module T, Reader, runReader) where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.ReaderT as R
import Unstable.Control.Monad.Trans as T     

type Reader r    = R.ReaderT r Identity

runReader         :: r -> Reader r a -> a
runReader r m     = runIdentity (R.runReader r m)


