module Unstable.Control.Monad.Reader (module T, Reader, runReader) where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.ReaderT as R
import Unstable.Control.Monad.Trans as T     
import Control.Monad.Fix

type Reader r     = R.ReaderT r Identity

runReader         :: r -> Reader r a -> a
runReader r m     = runIdentity (R.runReader r m)

{-
newtype Reader r a  = R (R.ReaderT r Identity a) 
                    deriving (Functor,Monad,MonadFix,MonadReader r)

instance HasBaseMonad (Reader r) (Reader r) where
  inBase            = id

runReader         :: r -> Reader r a -> a
runReader r (R m) = runIdentity (R.runReader r m)
-}




