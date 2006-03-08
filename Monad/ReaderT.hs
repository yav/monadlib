-- | An implementation of the reader (aka environment) monad transformer.
-- It is useful when computations need to access some immutable context.
--
--
-- * Commutes with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT"
-- 
-- * Does not commute with: "Monad.ExceptT"
module Monad.ReaderT 
  ( -- * Instance notes

    -- ** instance 'HandlerM'
    -- $HandlerM

  ReaderT, runReader, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix

-- | A computation that computes a value of type /a/, 
-- may use a context of type /r/,
-- and my side-effect as described by /m/.
newtype ReaderT r m a = R (r -> m a) 

-- | Execute a computation in the given context.
runReader          :: r -> ReaderT r m a -> m a
runReader r (R m)   = m r

instance Functor m => Functor (ReaderT r m) where
  fmap f m          = R (\r -> fmap f (runReader r m))

instance Monad m => Monad (ReaderT r m) where
  return a          = R (\_ -> return a)
  m >>= k           = R (\r -> (runReader r . k) =<< runReader r m)

instance BaseM m b => BaseM (ReaderT r m) b where
  inBase m          = lift (inBase m)

instance Trans (ReaderT r) where
  lift m            = R (\_ -> m)

instance MonadFix m => MonadFix (ReaderT r m) where
  mfix f            = R (\r -> mfix (runReader r . f))

instance MonadPlus m => MonadPlus (ReaderT r m) where
  mzero             = lift mzero
  mplus (R f) (R g) = R (\r -> mplus (f r) (g r))

instance Monad m => ReaderM (ReaderT r m) r where
  getR              = R (\r -> return r)

instance Monad m => ReadUpdM (ReaderT r m) r where
  updateR f m       = R (\r -> runReader (f r) m)

instance WriterM m w => WriterM (ReaderT r m) w where
  put o             = lift (put o)

instance CollectorM m w => CollectorM (ReaderT r m) w where
  collect m         = R (\r -> collect (runReader r m))

instance StateM m s => StateM (ReaderT r m) s where
  get               = lift get
  set s             = lift (set s)

instance ExceptM m e => ExceptM (ReaderT r m) e where
  raise e           = lift (raise e)

-- $HandlerM
-- When an exception is thrown the context in the handler is restored.
-- For example:
-- 
-- > test = runId $ runExcept $ runReader 42  
-- >      $ withHandler (\_ -> getR) 
-- >      $ setR 17 (raise "Error") 
-- 
-- produces @Right 42@

instance HandlerM m e => HandlerM (ReaderT r m) e where
  handle m h        = R (\r -> withHandler (runReader r . h)
                             $ runReader r m) 
                                

