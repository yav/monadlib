-- | An implementation of the reader (aka environment) monad transformer.
-- Implements the ability to pass around some context.
--
-- * Commutes with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT"
-- * Does not commute with: "Monad.ExceptT", "Monad.SearchT", "Monad.ContT"
module Monad.ReaderT 
  ( -- * Instance notes

    -- ** instance ExceptM
    -- $ExceptM

    -- ** instance SearchM
    -- $SearchM

    -- ** instance ContM
    -- $ContM

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

instance Monad m => ReaderM (ReaderT r m) r where
  get               = R (\r -> return r)
  local f m         = R (\r -> runReader (f r) m)

instance WriterM m w => WriterM (ReaderT r m) w where
  put o             = lift (put o)

instance TakeWriterM m w => TakeWriterM (ReaderT r m) w where
  takeFrom m        = R (\r -> takeFrom (runReader r m))

instance StateM m s => StateM (ReaderT r m) s where
  peek              = lift peek
  poke s            = lift (poke s)

-- $ExceptM
-- When an exception is thrown the context in the handler is restored.
-- see: <Examples/Reader/Except.hs>
instance ExceptM m e => ExceptM (ReaderT r m) e where
  raise e           = lift (raise e)
  handle m h        = R (\r -> handle (runReader r m) 
                                (\x -> runReader r (h x))) 

-- $SearchM
-- mplus: When a reader backtracks the context is restored.
-- In other words, context changes are local to the alternative.
-- 
-- 'findOne': The computation returned by 'findOne' to produce more answers
-- uses the environment at the time when 'findOne' was applied.
-- Using 'local' on that compuation has no effect.
--
-- see: <Examples/Reader/Search.hs>
instance MonadPlus m => MonadPlus (ReaderT r m) where
  mzero               = lift mzero
  -- We do not need the context when we have no results.

  mplus m1 m2         = R (\r -> runReader r m1 `mplus` runReader r m2)
  -- Different alternatives share the same context.

instance SearchM m => SearchM (ReaderT r m) where
  force m             = R (\r -> force (runReader r m))

  findOne m           = R (\r -> (up # ) # findOne (runReader r m))
    where
    up (x,xs)         = (x,lift xs)
  -- All the searching happens in the context that was active when
  -- 'findOne' was invoked.

-- $ContM
-- When we jump to a captured continuation the context is restored.
--
-- see: <Examples/Reader/Cont.hs>
instance ContM m => ContM (ReaderT r m) where
  callcc m          = R (\r -> callcc 
                            (\k -> runReader r (m (\a -> lift (k a)))))










