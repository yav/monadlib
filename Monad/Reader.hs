-- | Implements the reader (aka environment) monad.
module Monad.Reader (Reader, runReader, module Monad.Prelude) where

import Monad.Prelude
import Control.Monad.Fix


-- | A computation that computes a value of type /a/,
-- and can access a context of type /r/.
newtype Reader r a  = R (r -> a)

-- | Execute the computation in the given context.
runReader          :: r -> Reader r a -> a
runReader r (R m)   = m r 

instance Functor (Reader r) where
  fmap f (R m)      = R (\r -> f (m r))

instance Monad (Reader r) where
  return x          = R (\_ -> x)
  R m >>= k         = R (\r -> let R n = k (m r)
                               in n r)

instance BaseM (Reader r) (Reader r) where inBase x = x

instance MonadFix (Reader r) where
  mfix f            = R (\r -> let R n = f a
                                   a   = n r
                               in a)

instance ReaderM (Reader r) r where
  get               = R (\r -> r)
  local f (R m)     = R (\r -> m (f r))



