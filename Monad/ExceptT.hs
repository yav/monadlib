module Monad.ExceptT 
  ( ExceptT, Except(..)
  , run
  , raise, handle
  ) where

import Control.Monad.Fix
import Monad.Except(Except(..))

newtype ExceptT e m a  = E (m (Except e a))

instance Functor m => Functor (ExceptT e m) where
  fmap f (E m)        = E (fmap (fmap f) m) 

instance Monad m => Monad (ExceptT e m) where
  return a          = E (return (Ok a))
  E m1 >>= k        = E (do x <- m1
                            case x of
                              Fail e  -> return (Fail e)
                              Ok a    -> let E m2 = k a 
                                         in m2)

instance MonadFix m => MonadFix (ExceptT e m) where
  mfix f            = E (mfix (\ ~(Ok a) -> let E m = f a in m))


-- | Run a computation, an answer on the left indicates an error occured,
-- | an answer on the right indicates a successful execution.
run                :: ExceptT e m a -> m (Except e a)
run (E m)           = m

-- | Raise an exception.
raise              :: Monad m => e -> ExceptT e m a
raise e             = E (return (Fail e))

-- | Handle an exception. Do nothing if no exception occurs.
handle             :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
handle (E m1) h     = E (do x <- m1
                            case x of
                              Fail e  -> let E m2 = h e 
                                         in m2
                              _       -> return x)



