-- | Shows how the different transformers that affect the control flow of
-- a program interact with each other.
module Examples.Control where

import Monad.ExceptT
import Monad.BackT
import Monad.ContT


testBackExcept ()   = (raise () `mplus` return 2) `handle` \_ -> mzero

runBackExceptIn     = (runExcept $ findOne depthFirst $ testBackExcept ()) >>= print 
runBackExceptOut    = (findOne depthFirst $ runExcept $ testBackExcept ()) >>= print 


