module Monad.Writer 
  ( Writer
  , run, eval, exec
  , put, takeFrom
  ) where

import Control.Monad.Fix


newtype Writer w a  = W (w -> (w -> w -> w) -> (a,w))

instance Functor (Writer w) where
  fmap f (W m)      = W (\n j -> let (a,w) = m n j
                                 in (f a, w))

instance Monad (Writer w) where
  return a          = W (\n _ -> (a,n))
  W m1 >>= k        = W (\n j -> let (a,w1) = m1 n j 
                                     W m2   = k a
                                     (b,w2) = m2 n j
                                 in (b, j w1 w2))

instance MonadFix (Writer w) where
  mfix f            = W (\n j -> let W m      = f a
                                     r@(a,w)  = m n j
                                 in r)

-- | Execute a computation.
run                :: w -> (w -> w -> w) -> Writer w a -> (a,w)
run n j (W m)       = m n j

-- | Execute a computation, ignoring the output.
eval               :: w -> (w -> w -> w) -> Writer w a -> a
eval n j m          = fst (run n j m)

-- | Execute a computation for its side-effect.
exec               :: w -> (w -> w -> w) -> Writer w a -> w
exec n j m          = snd (run n j m)

-- | Put data on the output.
put                :: w -> Writer w ()
put w               = W (\_ _ -> ((),w))

-- | Get the data output by a computation. The new computation contains no data.
takeFrom           :: Writer w a -> Writer w (a,w)
takeFrom (W m)      = W (\n j -> (m n j, n))

                                         


