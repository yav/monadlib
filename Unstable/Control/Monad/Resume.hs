module Unstable.Control.Monad.Resume (Resume, hyper, module T) where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.ResumeT as R
import Unstable.Control.Monad.Trans as T     

type Resume   = R.ResumeT Identity

hyper         :: Resume a -> a
hyper m       = runIdentity (R.hyper m)


