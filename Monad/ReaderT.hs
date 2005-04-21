-- | An implementation of the reader (aka environment) monad transformer.
-- Implements the ability to pass around some context.
--
-- * Commutes with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT"
--
-- * Does not commute with: "Monad.ExceptT", "Monad.BackT", "Monad.ContT"
module Monad.ReaderT 
  ( -- * Instance notes

    -- ** instance ExceptM
    -- $ExceptM

    -- ** instance BackM
    -- $BackM

    -- ** instance ContM
    -- $ContM

  ReaderT, runReader, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix

-- | A computation that computes a value of type /a/, may use a context of type /r/,
-- and my side-effect as described by /m/.
newtype ReaderT r m a = R (r -> m a) 

-- | Execute a computation in the given context.
runReader          :: r -> ReaderT r m a -> m a
runReader r (R m)   = m r

instance Functor m => Functor (ReaderT r m) where
  fmap f (R m)      = R (\r -> fmap f (m r))

instance Monad m => Monad (ReaderT r m) where
  return a          = R (\_ -> return a)
  R m1 >>= k        = R (\r -> do a <- m1 r
                                  let R m2 = k a
                                  m2 r)

instance BaseM m b => BaseM (ReaderT r m) b where
  inBase m          = lift (inBase m)

instance Trans (ReaderT r) where
  lift m            = R (\_ -> m)

instance MonadFix m => MonadFix (ReaderT r m) where
  mfix f            = R (\r -> mfix (\a -> let R m = f a 
                                           in m r))

instance Monad m => ReaderM (ReaderT r m) r where
  get               = R (\r -> return r)
  local f (R m)     = R (\r -> m (f r))

instance WriterM m w => WriterM (ReaderT r m) w where
  put o             = lift (put o)

instance TakeWriterM m w => TakeWriterM (ReaderT r m) w where
  takeFrom (R m)    = R (\r -> takeFrom (m r)) 

instance StateM m s => StateM (ReaderT r m) s where
  peek              = lift peek
  poke s            = lift (poke s)

-- $ExceptM
-- When an exception is thrown, the context in the handler is restored.
--
-- see: runReaderOut in <Examples/Except.hs>
instance ExceptM m e => ExceptM (ReaderT r m) e where
  raise e           = lift (raise e)
  handle (R m) h    = R (\r -> handle (m r) (\x -> let R m' = h x in m' r)) 

-- $BackM
-- When a reader backtracks the context is restored.
-- In other words, context changes are local to the alternative.
--
-- see: runReaderOut in <Examples/Back.hs>
instance MonadPlus m => MonadPlus (ReaderT r m) where
  mzero               = lift mzero
  mplus (R m1) (R m2) = R (\r -> m1 r `mplus` m2 r)

-- $ContM
-- When we jump to a captured continuation the context is restored.
--
-- see: runReaderOut in <Examples/Cont.hs>
instance ContM m => ContM (ReaderT r m) where
  callcc m          = R (\r -> callcc (\k -> let R m' = m (\a -> lift (k a)) in m' r))










